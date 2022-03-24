function registerDrop(dropped) {
    var f = window.top;

    function prevent(e) {
        e = e || event;
        e.preventDefault();
    }

    function addprevent(el) {
        el.addEventListener("dragover",function(e){prevent(e)},false);
        el.addEventListener("drop",function(e){prevent(e)},false);
    }
    addprevent(f);
    
    var ddd = "inline";
    var hidden = false;
    function hideiframes() {
        if(!hidden) {
            for (let d of f.document.getElementsByTagName('iframe')) { ddd = d.style.display; d.style.display = "none" };
            hidden = true;
        }
    }

    function showiframes() {
        if(hidden) {
            for (let d of f.document.getElementsByTagName('iframe')) { d.style.display = ddd };
            hidden = false;
        }
    }

    var dropping = false;
    var insideframes = 0;

    function dragenter(e) {
        e.stopPropagation();
        e.preventDefault();
        dropon(e);
        hideiframes();
    }
    function dragleave(e) {
        f.setTimeout(function() {
            if(insideframes===0){
                e.stopPropagation();
                e.preventDefault();
                dropcancel();
                showiframes();
            } 
        },0);
    }
    
    function drop(e) {
        e.stopPropagation();
        e.preventDefault();
        dropped(e,dropping);
        dropcancel();
        showiframes();
        insideframes = 0;
    }

    function dragenterframe(e) {
        insideframes = insideframes+1;
        if(insideframes<0) insideframes=0;
        dragenter(e);
    }
    function dragleaveframe(e){
        insideframes = insideframes-1;
        if(insideframes<0) insideframes=0;
        dragleave(e);
    }
    function resetframe(){
        dropcancel();
        showiframes();
        var nott = f.document.getElementById("dropper_notification");
        if(nott){f.document.body.removeChild(nott)};
        dropping = false;
        insideframes = 0;
    }

    f.document.addEventListener('dragenter', function(e) { dragenter(e); });
    f.document.addEventListener('dragleave', function(e) { dragleave(e); });
    f.document.addEventListener('drop', function(e) { drop(e); });

    function registerframe(el) {
        el.addEventListener('dragenter', function(e) { dragenterframe(e); });
        el.addEventListener('dragleave', function(e) { dragleaveframe(e); }); 
        el.addEventListener('drop', function(e) { drop(e); });
    }

    for (let d of f.document.getElementsByTagName('iframe')) 
        { 
            addprevent(d);
            registerframe(d);
            registerframe(d.contentWindow.document);
            addprevent(d.contentWindow.document);
        };

    function dropon(e) {
        if (!dropping && e.dataTransfer.items.length > 0 && e.dataTransfer.items[0].kind === "file") {
            var t1 = document.createElement("span");

            t1.innerHTML = "<i class='ui inverted icon folder outline'> </i>";
            t1.setAttribute("style","font-size:60px;color:white;width:100%;height:100%;display:block;text-align:center;padding-top:calc(50vh - 30px);background-color:rgba(83,83,83,0.5);z-index:999999;top:0;left:0;position:fixed;user-select:none;");
            t1.setAttribute("id","dropper_notification");
            t1.addEventListener("click",function(){setTimeout(function(){resetframe();},0)});
            registerframe(t1);
            f.document.body.appendChild(t1);
            dropping = true;
            
        }
    };
    
    function dropcancel() {
        if (dropping) {
            f.document.body.removeChild(f.document.getElementById("dropper_notification"));
            dropping = false;
        }
    };
};