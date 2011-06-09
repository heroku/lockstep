var port = parseInt(process.env["PORT"] || '4567')
var http = require('http');
var sys = require('sys');
http.createServer(function (req, res) {
  console.log("URL: " + req.url);
  console.log("incoming connection");
  res.writeHead(200, {'Content-Type': 'text/plain'});
  setInterval(function() {
    res.write(JSON.stringify( {id: 123, updated_at: 999, deleted_at: null, ip: "0.0.0.0", port: 1234}))
  }, 1);
  setInterval(function() {
    res.write(JSON.stringify( {id: 124, updated_at: 1000, deleted_at: 1000, ip: "0.0.0.0", port: 1234}))
  }, 1);
}).listen(port, "0.0.0.0");
console.log("Listening on port " + port);
