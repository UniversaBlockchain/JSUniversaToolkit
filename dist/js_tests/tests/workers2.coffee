test = window.unitjs || {}

dec64 = Universa.utils.v2.decode64
enc64 = Universa.utils.v2.encode64

Unitest.addTestModule ->

   Unitest.addTest 'test pk generator in worker', ->

        pkCallback3 = (err, pk) ->
          #if (err?)
          #    console.log( 'Private key generation failed with usual worker!');
          #    console.log( err);
              # assertTrue is not defined here :(
          #else
              console.log( 'Private key was generated with usual worker!');
              console.log( pk);
              # assertTrue is not defined here :(

        # works with local web server like "python -m SimpleHTTPServer"

        wname = Universa.Worker3.init('http://localhost:8000', {}, pkCallback3);
        Universa.Worker3.run(wname, { command: 'generatePrivateKey', bits: 2048, exp: 17 })
        # Universa.Worker3.destroy(wname)

   Unitest.addTest 'test load items in worker', ->
        APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
        TEST_APP_TOKEN = "X19URVNUSU5HX1RPS0VOX2xramhsZGtqcWUhQCRsa2poYjMyMDk4NDFkdWIkOTE3LWghbGtqa2wsaTk="
        PACKED_CHANGING_ACC_KEY = dec64 '''
            JgAcAQABxAAB6X7cH9NdSxJ1rR/7QeRmDCWM0qNIJkQnI/T8kIAFt2VElm+7
            XeOEpN7tJC85dddWN6hegqW5FrJ8Ug2w8wBuseb/nZpEPeXzKjnAGpd7vrx3
            qfrvQirjCKyVE6OyseLGG1RXvcMTseqdCLAJz/a00SdgqRjK5zH6BhCJiRzV
            8tBsycopGrtPDbHbiSpgmYqvk63nLAxUrD6K/ZdfIN2P7HYekN9Um16L8e9U
            Ro8oqxTAv5kVLr04pA0GajBXl6jUa5Kp/xawSJmOeWY7Hpoi3u2zUa/sMs7O
            RivG9Hbvmj/S89wCjyFd0etLsdT1DH5bnZqWY34pFNuSqOvUKF8AdcQAAfjR
            0ILGcT0oRWN+oa5veOJy0icrk+KpCtDOcDSBLB6glU2HuS75WDJhlWDKcjBC
            m+JdpDRvc+6ISiDs3uUwoMz49mOkGriGJgMwUAnn+o2k+4aL6f2xfOLpGOio
            kKwGXg86zQLFD20qqToxfrZFvzjmVtM9msuNxeJjJtt/2tx6iMogaql8B6Cq
            JLTYuKdb+aJPp8oGNit2ofsp7nbzSKSAXWAX3d25H8HAhJ+xDCJ3r0gmRCcj
            9PydN2XsqHGWb7G9Rs4H0HgXndP9/fHjyiPLa/15BuiluRay4VJnmhFR0Tjr
            EL+nURLBubWit2VY/I0GxfDMdlwz3qi00lLW7ss=
          '''
        DP_KEY = dec64 '''JgAcAQABvIEA77LCIRQU/WWW2VZ6Djoz0CEKlqxqMKonkCvDhQns+QCt/zCiLxcI5RiJbIvfZjUs6ET9P2mO+9OeJodIO5GaSLesv3EMS7vfPKbuCd+BWcGegT0nRSkk0L7+2HQb2MRWPNrVM++sinvkM6zNmQuBdlUUpi+D9MnjcFk0ZrDqX2u8gQDiGgvOGSSAJbFa4/HmCgWZffXGgygnY84CP0GAP4FAmzEzGlpiLZ10rLaqnM62FSdEh9X5xxtao+M5DrTFN2iB5lVzjigN7j8OROgmYjh9hEVFYTGEWhABwziX0kSs90EqaSk393U45ZbtMBD/dnl/GYpnVuZ75uiLskBsW9DgMw=='''


        pkCallback3 = (err, items) ->
          #if (err?)
          #    console.log( 'Items loading failed with worker!');
          #    console.log( err);
              # assertTrue is not defined here :(
          #else
              console.log( 'Items were loaded with worker!');
              console.log( await items);
              # assertTrue is not defined here :(

        # works with local web server like "python -m SimpleHTTPServer"

        #api = await CryptoCloud.connect TEST_APP_TOKEN, PACKED_CHANGING_ACC_KEY
        api = await CryptoCloud.connect APP_TOKEN, DP_KEY

        wname = Universa.Worker3.init('http://localhost:8000', {'api': api}, pkCallback3);
        Universa.Worker3.run(wname, { command: 'loadItems', sessionToken: api.sessionToken })
        # Universa.Worker3.destroy(wname)

