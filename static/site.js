$("#btn-evaluate").click(function(){
    var codeContent=$("#code-textarea").val();
    callService("http://localhost:8088/eval-code",
		{code:codeContent},
		evalCodeCallback);
});

function evalCodeCallback(data,status){
    console.log(status + " " + data);
}

function callService(serviceURL, params, callbackFn){
    $.ajax({
	url: serviceURL,
	type: 'POST',
	data: params,
	success: callbackFn
    });
}
