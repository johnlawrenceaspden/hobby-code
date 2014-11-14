(function() {
var x=document.querySelectorAll('*');
for(var i=0;i<x.length;i++){
    if(x[i].style.position=='fixed'){
        x[i].style.position='static';
    }};
})();
