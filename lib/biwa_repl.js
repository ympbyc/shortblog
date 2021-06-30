window.biwa_interpreter = (function () {
    var biwa_interpreter = new BiwaScheme.Interpreter((err)=>{
        sock.send("ERROR: " + err);
    });
    var resp = null;
    var sock = new WebSocket('ws://127.0.0.1:8001');

    sock.addEventListener('open',function(e){
        console.log('WebSocket connection established');
        sock.send("Got a client connected.");
    });

    sock.addEventListener('message',function(e){
        console.log(e.data);
        biwa_interpreter.evaluate(e.data, function (result) {
            console.log(result);
            sock.send(result);
        });
    });
    return biwa_interpreter;
})();

