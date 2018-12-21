#!/bin/bash
coffee --version
coffee -c -t --watch --output js tests/*.coffee
