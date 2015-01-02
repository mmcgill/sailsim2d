// sailsim2d.js
// WebSocket-based browser client for SailSim 2D

var sailsim = (function () {
    var my = {};

    function getBoat () {
        if (my.state != null) {
            return my.state.boats[my.id];
        } else {
            return null;
        }
    };

    ////////////////////////////////////////////
    // message handlers
    function handleState (newState) {
        my.state = newState;
    };

    function handleId (id) {
        my.id = id;
    };

    ////////////////////////////////////////////
    // WebSocket management
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

    ////////////////////////////////////////////
    // Rendering

    function drawArrow (ctx, x1, y1, x2, y2) {
        ctx.beginPath();
        ctx.moveTo(x1, y1);
        ctx.lineTo(x2, y2);
        ctx.stroke();
    }
    
    function drawBoat (ctx, boat) {
        var posx=boat.pos[0], posy=boat.pos[1], vx=boat.v[0], vy=boat.v[1];
        ctx.save();
        ctx.rotate(boat.theta);
        ctx.drawImage(document.getElementById("boat"), posx-1.8, posy-0.7, 3.6, 1.4);
        ctx.restore();
        // draw velocity
        ctx.strokeStyle = "red";
        ctx.lineWidth = 0.1;
        drawArrow(ctx, posx, posy, posx+vx, posy+vy);
    }
    
    function drawFrame (timestamp) {
        var ctx = document.getElementById("sailsim_canvas").getContext("2d");
        ctx.save();
        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
        if (my.state != null) {
            var b = getBoat();
            var posx=b.pos[0], posy=b.pos[1];
            ctx.strokeText("x: " + String(posx).substring(0,5) +
                           " y: " + String(posy).substring(0,5), 10, 30);
            ctx.translate(ctx.canvas.width/2.0 - posx*10, ctx.canvas.height/2.0 - posy*10);
            //ctx.translate(posx, posy);
            ctx.scale(10, 10); // 10 pixels per meter
            for (var id in my.state.boats) {
                drawBoat(ctx, my.state.boats[id]);
            }
        }
        ctx.restore();

        window.requestAnimationFrame(drawFrame);
    };

    window.requestAnimationFrame(drawFrame);
    
    return my;
})();
