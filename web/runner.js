
function sendHttp(method, url, body, callback) {
    var request = new XMLHttpRequest();

    if (callback) {
        request.onreadystatechange = function() {
            if (request.readyState == 4) callback(request);
        };
    }

    request.open(method, url, true);
    request.send(body);
}

run = function(){
  var editor = ace.edit("aceEditor");
  var code = editor.getValue();

  sendHttp('GET', 'clientId.txt', null, function(request) {
      if (request.status != 200 || request.responseText == '') {
          sweetAlert('Oops!', 'Missing API client key.  You will not be able to sign in.', 'warning');
          return null;
      }

      window.clientId = request.responseText.trim();
      return f(window.clientId);
  });
}
