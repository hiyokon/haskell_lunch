var Main = (function(){
    
    function Main(){
        root = this;
        this.delay = null;
    
        window.onload = function(){
            // Util Part
//          root.destroyer = new Destroyer();
//          root.destroyer.attachMethodsToClass([Utility],Array);
//          root.destroyer.attachMethodsToClass([Array],NodeList);
            
            // Leap Part
            root.leapWrapper = new LeapWrapperForImpress();
            root.startMotionCapture();
        }
    }
    
    Main.prototype.startMotionCapture = function(){
        var root = this;
        
        // Listen to Leap Motion
        Leap.loop({enableGestures: true}, function(frame){
            var gestures = frame.gestures;
            if (gestures.length > 1){
                var headGesture = gestures[0];
                if (headGesture.state == "stop") {
                    console.log(headGesture);
                    switch (headGesture.type) {
                        case 'circle':
                            root.leapWrapper.gotoOverview();
                            break;
                        case 'swipe':
                            if (headGesture.direction[0] > 0) {
                                root.leapWrapper.gotoNext();
                            //  setTimeout(function(){},5000);
                                break;
                            } else {
                                root.leapWrapper.gotoPrevious();
                                break;
                            }
                        case 'screenTap':
                            break;
                        case 'keyTap':
                            break;
                        default:
                            break;
                    }
                }
            }
        });
        
    }
    
    Main.prototype.stopMotionCapture = function(){
    }
    
    return Main;
})();

new Main();



// ---------------------------------------------------
// Carrier

//          var outputDiv = document.getElementById("output")
//          function output(json){
//              var div = document.createElement("div");
//              div.innerHTML = JSON.stringify(json) + "<br/>";
//              outputDiv.insertBefore(div, outputDiv.firstChild);
//          };

//
//            if(gesturesLength > 0){
//                frame.gestures.forEach(function(gesture){
//                    console.log(gesture);
//                });
//            }

//            $(frame.gestures).each(function(){
//                typeValue.html(this.type);
//                stateValue.html(this.state);
//            });
