var Trianglify = require('trianglify');

exports.setBackground = function (i) {
return function () {
var pattern = Trianglify({
        width: window.innerWidth,
        height: window.innerHeight,
        x_colors: 'RdGy',
        variance: 0.6 + 20/(i + 50),
        cell_size: i + 50
    });
var c = pattern.canvas();
c.style.position = 'fixed';
c.style.zIndex = -1;
//document.getElementById("app").style.position = 'absolute';
//document.body.style.margin = 0;
document.getElementById("bk").appendChild(c);
}
}
