// sailsim2d.js
// WebSocket-based browser client for SailSim 2D

var sailsim = (function () {
    var my = {};

    function getBoat () {
        if (my.state != null) {
            return my.state.objects[my.id];
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
    function sendMsg(tag,body) {
        my.socket.send(JSON.stringify([tag, body]));
    }

    ////////////////////////////////////////////
    // keyboard input
    var PI = 3.14159;
    window.addEventListener('keydown', function (e) {
        switch (e.keyCode) {
        case 37: sendMsg("set-rudder-theta", PI/8); break;
        case 39: sendMsg("set-rudder-theta", -PI/8); break;
        }
    });
    window.addEventListener('keyup', function (e) {
        switch (e.keyCode) {
        case 37: sendMsg("set-rudder-theta", 0); break;
        case 39: sendMsg("set-rudder-theta", 0); break;
        }
    });
       

    ////////////////////////////////////////////
    // Rendering

    function drawArrow (ctx, x1, y1, x2, y2) {
        ctx.beginPath();
        ctx.moveTo(x1, y1);
        ctx.lineTo(x2, y2);
        ctx.stroke();
    }

    function clampToGrid(n,size) {
        return Math.floor(n/size)*size;
    }

    function drawVectorGrid(ctx, center, size, v) {
        var screenWidthInMeters = ctx.canvas.width / my.pixelsPerMeter;
        var screenHeightInMeters = ctx.canvas.height / my.pixelsPerMeter;
        var startX = clampToGrid(center[0]-(screenWidthInMeters/2), size);
        var startY = clampToGrid(center[1]-(screenHeightInMeters/2), size);
        var cols = Math.ceil(screenWidthInMeters/size);
        var rows = Math.ceil(screenHeightInMeters/size);
        for (var i = 0; i <= cols; i++) {
            var x = startX + (i * size);
            for (var j = 0; j <= rows; j++) {
                var y = startY + (j * size);
                drawArrow(ctx, x, y, x+v[0], y+v[1]);
            }
        }
    }
    
    function drawBoat (ctx, boat) {
        var posx=boat.pos[0], posy=boat.pos[1], vx=boat.v[0], vy=boat.v[1];
        ctx.save();
        ctx.translate(posx,posy);
        ctx.rotate(boat.theta);
        ctx.drawImage(document.getElementById("boat"), -1.8, -0.7, 3.6, 1.4);
        ctx.restore();
        // draw velocity
        ctx.strokeStyle = "red";
        ctx.lineWidth = 1/my.pixelsPerMeter;
        drawArrow(ctx, posx, posy, posx+vx, posy+vy);
    }
    
    my.pixelsPerMeter = 8;
    function drawFrame (timestamp) {
        var ctx = document.getElementById("sailsim_canvas").getContext("2d");
        ctx.save();
        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
        if (my.state != null) {
            var b = getBoat();
            var posx=b.pos[0], posy=b.pos[1];
            ctx.strokeText("x: " + String(posx).substring(0,5) +
                           " y: " + String(posy).substring(0,5), 10, 30);
            ctx.translate(ctx.canvas.width/2.0 - posx*my.pixelsPerMeter,
                          ctx.canvas.height/2.0 - posy*my.pixelsPerMeter);
            ctx.scale(my.pixelsPerMeter, my.pixelsPerMeter);
            // draw water current on a grid with vertices on multiples of 50 meters
            ctx.strokeStyle = "blue";
            ctx.lineWidth = 1/my.pixelsPerMeter;
            drawVectorGrid(ctx, b.pos, 10, my.state.current);
            for (var id in my.state.objects) {
                if ("boat" == my.state.objects[id].type) {
                    drawBoat(ctx, my.state.objects[id]);
                }
            }
        }
        ctx.restore();

        window.requestAnimationFrame(drawFrame);
    };

    window.requestAnimationFrame(drawFrame);
    
    return my;
})();
