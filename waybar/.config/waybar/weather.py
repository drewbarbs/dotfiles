#!/usr/bin/env python3
import argparse
import functools
import json
import sys

from datetime import timedelta

import dbus
import gi
import requests

gi.require_version('GLib', '2.0')
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib

COLOR_BLUE = 'lightblue'
COLOR_GREEN = '#CEFFAC'
COLOR_RED = '#FFB6B0'


def color_text(text, color):
    return f'<span foreground="{color}">{text}</span>'


def update_weather(latitude, longitude):
    update = {}
    resp = requests.get(
        f'https://api.weather.gov/points/{latitude:.4f},{longitude:.4f}')
    if not resp.ok or 'properties' not in (resp_obj := json.loads(resp.text)):
        update['text'] = 'Failed to fetch gridpoints'
        update['class'] = 'error'
    else:
        points = resp_obj['properties']
        hrly_forecast_url = points['forecastHourly']
        relative_loc = points['relativeLocation']['properties']
        city, state = relative_loc['city'], relative_loc['state']

        update['tooltip'] = f'{city}, {state}'

        resp = requests.get(hrly_forecast_url)
        if not resp.ok or 'properties' not in (resp_obj := json.loads(resp.text)):
            update['text'] = 'Failed to fetch gridpoints'
            update['class'] = 'error'
        else:
            forecast = json.loads(resp.text)['properties']
            this_hr = forecast['periods'][0]

            temp = this_hr['temperature']
            unit = this_hr['temperatureUnit']
            short_forecast = this_hr['shortForecast']

            if temp < 60:
                color = COLOR_BLUE
            elif temp < 77:
                color = COLOR_GREEN
            else:
                color = COLOR_RED

            update['text'] = f'{color_text(temp, color)}°{unit} {short_forecast}'

    print(json.dumps(update), flush=True)
    return True


def location_handler(bus, cli, _, loc_path):
    props = dbus.Interface(
        bus.get_object('org.freedesktop.GeoClue2', loc_path),
        'org.freedesktop.DBus.Properties')
    longitude = float(
        props.Get('org.freedesktop.GeoClue2.Location', 'Longitude'))
    latitude = float(props.Get('org.freedesktop.GeoClue2.Location',
                               'Latitude'))
    accuracy = props.Get('org.freedesktop.GeoClue2.Location', 'Accuracy')
    description = props.Get('org.freedesktop.GeoClue2.Location', 'Description')
    cli.Stop()

    callback = functools.partial(update_weather, latitude, longitude)
    callback()
    GLib.timeout_add_seconds(int(timedelta(minutes=30).total_seconds()),
                             callback)


def main():
    DBusGMainLoop(set_as_default=True)

    bus = dbus.SystemBus()
    loop = GLib.MainLoop()

    # exit on stdout close
    GLib.io_add_watch(sys.stdout, GLib.PRIORITY_DEFAULT,
                      GLib.IO_HUP | GLib.IO_ERR, lambda *args: loop.quit())

    print('Updating...', flush=True)

    manager = bus.get_object('org.freedesktop.GeoClue2',
                             '/org/freedesktop/GeoClue2/Manager')
    client_path = manager.GetClient(
        dbus_interface='org.freedesktop.GeoClue2.Manager')
    client = dbus.Interface(
        bus.get_object('org.freedesktop.GeoClue2', client_path),
        'org.freedesktop.GeoClue2.Client')
    client.Set('org.freedesktop.GeoClue2.Client',
               'DistanceThreshold',
               dbus.UInt32(10000),
               dbus_interface='org.freedesktop.DBus.Properties')
    client.Set('org.freedesktop.GeoClue2.Client',
               'DesktopId',
               'XmobarWeather',
               dbus_interface='org.freedesktop.DBus.Properties')

    client.connect_to_signal('LocationUpdated',
                             functools.partial(location_handler, bus, client))

    try:
        client.Start()
    except dbus.exceptions.DBusException as e:
        # One way to fix this error on Arch was to start the geoclue demo agent:
        # dex /usr/share/applications/geoclue-demo-agent.desktop
        print(json.dumps({
            'text': 'Error connecting to DBus',
            'tooltip': str(e),
            'class': 'error'
        }), flush=True)
        return

    loop.run()

    print('done')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--pango', default=False, action='store_true')

    args = parser.parse_args()
    if args.pango:
        FORMAT = 'pango'

    main()
