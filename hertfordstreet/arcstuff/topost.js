function toPost(getString) {
	var parms = getString.split('?'); 
	var newF = document.createElement("form"); 
	newF.action = parms[0]; 
	newF.method = 'POST'; 
	var parms = parms[1].split('&'); 
	for (var i=0; i<parms.length; i++) 
	{
		var pos = parms[i].indexOf('='); 
		if (pos > 0) {
			var key = parms[i].substring(0,pos); 
			var val = parms[i].substring(pos+1);  
			/*@cc_on @if (@_jscript)  var newH = document.createElement("<input name='"+key+"'>");  @else */  
			var newH = document.createElement("input"); 
			newH.name = key; 
			/* @end @*/ 
			newH.type = 'hidden'; 
			newH.value = val; 
			newF.appendChild(newH);
		}
	} 
	document.getElementsByTagName('body')[0].appendChild(newF); newF.submit();
}


