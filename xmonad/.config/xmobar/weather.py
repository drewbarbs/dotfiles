#!/usr/bin/env python3
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


def update_weather(latitude, longitude):
    resp = requests.get(f'https://api.weather.gov/points/{latitude:.4f},{longitude:.4f}')
    points = json.loads(resp.text)['properties']
    hrly_forecast_url = points['forecastHourly']
    relative_loc = points['relativeLocation']['properties']
    city, state = relative_loc['city'], relative_loc['state']

    resp = requests.get(hrly_forecast_url)
    forecast = json.loads(resp.text)['properties']
    this_hr = forecast['periods'][0]

    temp = this_hr['temperature']
    unit = this_hr['temperatureUnit']
    short_forecast = this_hr['shortForecast']

    if temp < 60:
        color = 'lightblue'
    elif temp < 77:
        color = '#CEFFAC'
    else:
        color = '#FFB6B0'

    print(f'<fc={color}>{temp}</fc>°{unit} {short_forecast}', flush=True)
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

    client.Start()
    loop.run()

    print('done')


if __name__ == '__main__':
    main()
