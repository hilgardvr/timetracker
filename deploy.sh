#!/bin/bash

echo "elm make src/Main.elm --output=main.js" &&
elm make src/Main.elm --output=main.js
echo "mv main.js ../be/timetracker-svc/public/javascripts" &&
mv main.js ../be/timetracker-svc/public/javascripts

