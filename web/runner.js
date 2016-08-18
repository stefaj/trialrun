
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
    var editor = ace.edit("editor");
    var code = editor.getValue();

    var data = new FormData();
    data.append('compiler', 'ghc'); 
    data.append('source', code);

    sendHttp('POST', 'compile', data, function(request) {
        if (request.status == 200) {
        }
        console.log(request.response);

    });
}
