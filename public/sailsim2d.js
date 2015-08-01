// sailsim2d.js
// WebSocket-based browser client for SailSim 2D

var sailsim = (function () {
    var my = {};
    my.entities  = {};
    my.pixelsPerMeter = 8;

    function getBoat () {
        if (my.entities != null) {
            return my.entities[my.boatId];
        } else {
            return null;
        }
    };
       

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
    
    function drawBoat (ctx) {
        var posx=this.pos[0], posy=this.pos[1], vx=this.v[0], vy=this.v[1];
        ctx.save();
        ctx.translate(posx,posy);
        ctx.rotate(this.theta);
        ctx.drawImage(document.getElementById("boat"), -1.8, -0.7, 3.6, 1.4);
        ctx.restore();
        // draw velocity
        ctx.strokeStyle = "red";
        ctx.lineWidth = 1/my.pixelsPerMeter;
        drawArrow(ctx, posx, posy, posx+vx, posy+vy);
    }

    function vadd(v1, v2) {
        return [v1[0]+v2[0], v1[1]+v2[1]];
    }

    function vmul(v1, v2) {
        return [v1[0]*v2[0], v1[1]*v2[1]];
    }

    function vnorm(v) {
        var l = Math.sqrt(v[0]*v[0]+v[1]*v[1]);
        return [v[0] / l, v[1] / l];
    }

    function vrot(v,theta) {
        var a = Math.sin(theta);
        var b = Math.cos(theta);
        return [(v[0] * b) - (v[1] * a), (v[0] * a) + (v[1] * b)];
    }

    function drawWakeCurve (ctx) {
        if (this.segments.length === 0)
            return;
        
        ctx.lineWidth = 0.5;
        ctx.lineCap = "round";
        ctx.globalCompositeOperation = "destination-over";
        ctx.beginPath();
        var seg = this.segments[0];
        ctx.moveTo(seg.pos[0], seg.pos[1]);
        for (var i=0; i<this.segments.length; i++) {
            seg = this.segments[i];
            var c = 255-Math.floor(255*Math.max(0,Math.min(seg.ttl / 200)));
            ctx.strokeStyle = "rgb("+c+","+c+",255)";
            ctx.lineTo(seg.pos[0], seg.pos[1]);
            ctx.stroke();
            ctx.beginPath();
            ctx.moveTo(seg.pos[0], seg.pos[1]);
        }
        var head = my.entities[this.headId];
        if (head != null) {
            var p = vadd(vrot([-head.length/2,0],head.theta),
                         head.pos);
            ctx.lineTo(p[0], p[1]);
        }
        ctx.stroke();
    }

    function drawBouy(ctx, bouy) {
        ctx.beginPath();
        ctx.arc(bouy[0], bouy[1], 0.5, 0, Math.PI*2, true);
        ctx.fill();
    }

    function drawCourse(ctx) {
        for (var i=0; i<this.segments.length; i++) {
            var segment = this.segments[i];
            ctx.strokeStyle = "red";
            drawBouy(ctx, segment.right);
            drawBouy(ctx, segment.left);
        }
    }

    function drawFrame (timestamp) {
        var ctx = document.getElementById("sailsim_canvas").getContext("2d");
        ctx.canvas.width = window.innerWidth;
        ctx.canvas.height = window.innerHeight;
        ctx.save();
        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
        var b = getBoat();
        if (b != null) {
            var posx=b.pos[0], posy=b.pos[1];
            ctx.strokeText("x: " + String(posx).substring(0,5) +
                           " y: " + String(posy).substring(0,5), 10, 30);
            ctx.strokeText("throttle: "+String(getBoat().throttle), 10, 50);
            ctx.translate(ctx.canvas.width/2.0 - posx*my.pixelsPerMeter,
                          ctx.canvas.height/2.0 - posy*my.pixelsPerMeter);
            ctx.scale(my.pixelsPerMeter, my.pixelsPerMeter);
            // draw water current on a grid with vertices on multiples of 50 meters
            ctx.strokeStyle = "blue";
            ctx.lineWidth = 1/my.pixelsPerMeter;
            drawVectorGrid(ctx, b.pos, 10, my.current);
            ctx.save();
            for (var id in my.entities) {
                my.entities[id].draw(ctx);
            }
            ctx.restore();
        }
        ctx.restore();

        window.requestAnimationFrame(drawFrame);
    };

    ////////////////////////////////////////////
    // client-side tick functions
    // TODO: Get from server!
    var secsPerTick = 1.0/30.0; 

    function tickWakeCurve(t) {
        for (var i=0; i < this.segments.length;) {
            var seg = this.segments[i];
            if (seg.ttl == 0) {
                this.segments.splice(i, 1);
            } else {
                seg.ttl -= 1;
                seg.pos = vadd(seg.pos, vmul(seg.v, [secsPerTick,secsPerTick]));
                i += 1;
            }
        }
        if (this.segments.length === 0) {
            delete my.entities[this.id];
        }
    }

    ////////////////////////////////////////////
    // message handlers
    function handleId (id) {
        my.id = id;
    };

    function handleSetBoatId (boatId) {
        my.boatId = boatId;
    }

    function handleBoatUpdate (update) {
        my.entities[update.id] = update;
        my.entities[update.id].draw = drawBoat;
        my.entities[update.id].tick = function(t) {};
    }

    function handleWakeSegment (seg) {
        var wakeCurve = my.entities[seg.id];
        if (wakeCurve == null) {
            wakeCurve = {
                id: seg.id,
                draw: drawWakeCurve,
                tick: tickWakeCurve,
                headId: seg['head-id'],
                segments: []
            };
            my.entities[seg.id] = wakeCurve;
        }
        wakeCurve.segments.push({pos: seg.pos, v: seg.v, ttl: seg.ttl});
    }

    function handleEnvUpdate(update) {
        my.wind = update.wind;
        my.current = update.current;
    }

    function handleTick(t) {
        my.t = t;
        for (var id in my.entities) {
            my.entities[id].tick(t);
        }
    }

    function handleCourse(course) {
        var c = {
            draw: drawCourse,
            tick: function(t) {},
            segments: course
        };
        my.entities["course"]=c;
    }

    function removeEntity(id) {
        delete my.entities[id];
    }

    var msgHandlers = {
        'set-boat-id': handleSetBoatId,
        'id': handleId,
        'boat-update': handleBoatUpdate,
        'env-update': handleEnvUpdate,
        'wake-segment': handleWakeSegment,
        'tick': handleTick,
        'course': handleCourse,
        'remove-entity': removeEntity
    };

    ////////////////////////////////////////////
    // WebSocket management
    my.socket = new WebSocket("ws://" + window.location.hostname + ":9090");
    my.socket.onmessage = function (e) {
        var msg = JSON.parse(e.data);
        var tag = msg[0], body = msg[1];
        var hdlr = msgHandlers[tag];
        if (hdlr != null) {
            hdlr(body);
        } else {
            console.log("Unrecognized tag '" + tag + "'");
        }
    };
    my.socket.onclose = function (e) {
        console.log("Connection to server has closed.");
    };
    function sendMsg(tag,body) {
        my.socket.send(JSON.stringify([tag, body]));
    }

    ////////////////////////////////////////////
    // keyboard/mouse input

    function clamp(min,max,v) {
        return Math.max(min, Math.min(max, v));
    }

    var PI = 3.14159;
    window.addEventListener('keydown', function (e) {
        switch (e.keyCode) {
        case 37: sendMsg("set-rudder-theta", PI/8); break;
        case 38: sendMsg("set-throttle", getBoat().throttle + 0.1); break;
        case 39: sendMsg("set-rudder-theta", -PI/8); break;
        case 40: sendMsg("set-throttle", getBoat().throttle - 0.1); break;
        }
    });
    window.addEventListener('keyup', function (e) {
        switch (e.keyCode) {
        case 37: sendMsg("set-rudder-theta", 0); break;
        case 39: sendMsg("set-rudder-theta", 0); break;
        }
    });
    window.addEventListener('wheel', function (e) {
        my.pixelsPerMeter -= clamp(-100,100,e.deltaY)/100.0;
        my.pixelsPerMeter = clamp(1,35,my.pixelsPerMeter);
    });

    // Start the animation loop
    window.requestAnimationFrame(drawFrame);
    
    return my;
})();
