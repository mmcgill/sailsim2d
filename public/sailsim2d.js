// sailsim2d.js
// WebSocket-based browser client for SailSim 2D

var sailsim = (function () {
    var my = {};

    function handleState (newState) {
        my.state = newState;
    };

    function handleId (id) {
        my.id = id;
    };

    
    my.socket = new WebSocket("ws://" + window.location.hostname + ":9090");
    my.socket.onmessage = function (e) {
        var msg = JSON.parse(e.data);
        var tag = msg[0], body = msg[1];
        if (tag == "state") handleState(body);
        else if (tag == "id") handleId(body);
        else console.log("Unrecognized tag '" + tag + "'");
    };
    my.socket.onclose = function (e) {
        console.log("Connection to server has closed.");
    };
    return my;
})();
