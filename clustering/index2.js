var simulation = d3.forceSimulation(nodes)
    // .force("charge", d3.forceManyBody().strength(-400))
    // .force("link", d3.forceLink(links).distance(30).strength(function (d) {
    .force("link",
           d3.forceLink(links).distance(30).strength(function (d) {
               return 0;
           }))
    //     return d.strength * d.strength;
    // }))
    // .force("collide", d3.forceCollide().radius(function(d) {
    //     return d.c;
    // }).strength(5))
    // .force("x", d3.forceX().strength(0.1))
    // .force("y", d3.forceY().strength(0.1))
    .on("tick", ticked);

var tau = 2 * Math.PI;

var canvas = document.querySelector("canvas"),
    context = canvas.getContext("2d"),
    width = canvas.width,
    height = canvas.height;

d3.select(canvas).on("click", function() {
    var subject = simulation.find(document.body.scrollLeft + d3.event.x - width / 2,
                                  document.body.scrollTop + d3.event.y - height / 2, 30);

    if (subject) {
        var url = "https://pypi.python.org/pypi/" + subject.label;
        var win = window.open(url, '_blank');
        win.focus();
    }
});


function ticked() {
    context.clearRect(0, 0, width, height);
    context.save();
    context.translate(width / 2, height / 2);

    // Draw links
    context.strokeStyle = "#ccc";
    context.beginPath();
    links.forEach(function(d) {
        // debugger;
        if (d.strength > 0.75) {
            context.moveTo(d.source.x, d.source.y);
            context.lineTo(d.target.x, d.target.y);
        }
    });
    context.stroke();

    // Draw nodes
    context.beginPath();
    nodes.forEach(function(d) {
        context.moveTo(d.x + d.r, d.y);
        context.arc(d.x, d.y, d.r, 0, tau);
    });
    context.fillStyle = "#ddd";
    context.fill();
    context.strokeStyle = "#333";
    context.stroke();

    // Draw labesls
    context.beginPath();
    context.textAlign = "center";
    nodes.forEach(function(d) {
        context.moveTo(d.x + d.r, d.y);
        context.strokeText(d.label,d.x,d.y);
    });


    context.restore();
}

function drawLink(d) {
    context.moveTo(d.source.x, d.source.y);
    context.lineTo(d.target.x, d.target.y);
}

function drawNode(d) {
    context.moveTo(d.x + 3, d.y);
    context.arc(d.x, d.y, d.r, 0, tau);
}

function clickSubject() {
    return simulation.find(d3.event.x - width / 2, d3.event.y - height / 2);
}

function onClick() {
    console.log(d3.event.subject.label);
}

d3.functor = function(v) {
    return typeof v === "function" ? v : function() { return v; };
};

// window.scrollTo(4172, 2851)

function getParameterByName(name, url) {
    if (!url) url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}

var cluster = getParameterByName("center")
if (cluster) {
    var node = nodes.find(function(node) { return node.label == cluster; });
    var scrollX, scrollY;
    if (node) {
        console.log(node);
        scrollX = node.x;
        scrollY = node.y;
        scrollX = width/2 + scrollX - window.innerWidth / 2;
        scrollY = height/2 + scrollY - window.innerHeight / 2;
        console.log(node);
        window.scrollTo(scrollX, scrollY);
    }
}
