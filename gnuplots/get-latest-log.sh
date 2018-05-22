#!/usr/bin/env bash


LOGDIR=$(ssh sw8@marand.dk "ls -r stonas-logs | head -1")
scp -r "sw8@marand.dk:stonas-logs/$LOGDIR" . && cp ./sum.p $LOGDIR && cp ./step-len.p $LOGDIR && cp ./delta-vs-len.p $LOGDIR
