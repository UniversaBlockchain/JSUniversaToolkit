# UniversaToolkit
JS and Scala.JS API to the crypto library, cloud and tools

To run unit tests you need to download [Chrome driver](https://sites.google.com/a/chromium.org/chromedriver/downloads)
and add it to your environment PATH variable.

To run tests with headless browser you need to install xvfb, "X Virtual FrameBuffer". For mac, you should install it from https://www.xquartz.org/
and restart your computer. Afterwards you can use it with sbt by
```
Xvfb :1 &
DISPLAY=:1 sbt
```


To get js api file:

`sbt “project crypto_cloud_api” fastOptJS`

js file with api is located in: crypto_cloud/target/scala-2.12/crypto_cloud_api-fastopt.js
