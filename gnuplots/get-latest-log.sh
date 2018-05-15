#!/usr/bin/env bash

scp -r sw8@marand.dk:stonas-logs/$(ssh sw8@marand.dk "ls -r stonas-logs | head -1") .
