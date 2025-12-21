#!/bin/bash

# Start dbus daemon if not running
if [ ! -e /var/run/dbus/pid ]; then
  mkdir -p /var/run/dbus
  dbus-daemon --system --fork 2>/dev/null || true
fi

# Export dbus address
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/dbus/system_bus_socket"

# Start the render server
exec npx tsx render-server.ts
