(function () {
    dec64 = Universa.utils.decode64;

    Unitest.addTestModule(function () {
        Unitest.addTest('import contact test', async function () {

            var xmlHttp = new XMLHttpRequest();
            xmlHttp.open( "GET", 'http://localhost:9900/readBytes', false ); // false for synchronous request
//            xmlHttp.overrideMimeType('text/plain; charset=x-user-defined');
            xmlHttp.send( null );
            var weGot = xmlHttp.responseText

            var privKey2 = Universa.PrivateKey.fromBase64(weGot);
            console.log(typeof(privKey2));


            var TEST_APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw==";
            var chatleApi = await Chatle.connect(TEST_APP_TOKEN, privKey2);
            var contacts = await chatleApi.loadItems({
                tags: ['contact'],
                limit: 5
            });
            console.log(contacts);


            //////////////////saving
            var xhr = new XMLHttpRequest();
            xhr.open("POST", "http://localhost:9900/writeBytes", false);
            xhr.send( weGot );
        });
    });
}).call(this);
