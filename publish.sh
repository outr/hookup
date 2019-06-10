#!/usr/bin/env bash

sbt +clean +test +hookupJS/publishSigned +hookupJVM/publishSigned sonatypeRelease +publishLocal