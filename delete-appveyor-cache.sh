#!/bin/bash

export APPVEYOR_TOKEN="put your token here"
curl -X "DELETE" -H "Authorization: Bearer $APPVEYOR_TOKEN" -H "Content-Type: application/json" https://ci.appveyor.com/api/projects/hughjonesd/huxtable/buildcache
