// ----------
// Class
// ----------

var LeapWrapperForImpress = (function(){
    // Constructor
    function LeapWrapperForImpress(){
    }
    
    return LeapWrapperForImpress;
})();

// ----------
// Method
// ----------

LeapWrapperForImpress.prototype.gotoNext = function(){
    impress().next();
};

LeapWrapperForImpress.prototype.gotoPrevious = function(){
    impress().prev();
};

LeapWrapperForImpress.prototype.gotoOverview = function(){
    var id = "overview";
    impress().goto(id);
};

LeapWrapperForImpress.prototype.gotoIndex = function(){
    var id = "index";
    impress().goto(id);
};

LeapWrapperForImpress.prototype.gotoPointing = function(frame){
    var finger = frame.fingers[0];
    var tipPosX = finger.tipPosition[0];
    var tipPosY = finger.tipPosition[1];
//  var pointedDiv = $("div").filter...
//  var id = getId(pointedDiv);
//  impress().goto(id);
};

function repeatedly(times,fun){
    return _.map(_.range(times),fun);
}

function best(fun,coll){
    return _.reduce(coll,function(x,y){
        return fun(x,y) ? x: y;
    });
}

var Hiyoko = (function(){
    function Hiyoko(){
    }
    return Hiyoko;
})();

Hiyoko.prototype.repeat = function(times,value){
    if (times == 0) {
        return [];
    } else {
        return [value].concat(this.repeat(times-1,value));
    }
};

Hiyoko.prototype.countFrequencyOn = function(array){
    var keys = _(array).uniq();
    var initVals = this.repeat(keys.length,0);
    var initObj  = _.object(keys,initVals);

    return _(array).reduce(function(memoObj,key){
        memoObj[key] += 1;
        return memoObj;
    },initObj);
};

Hiyoko.prototype.getModeOf = function(array){
    var obj = countFrequencyOn(array);
    return _.keys(obj).reduce(function(x,y){
        return (obj[x] > obj[y]) ? _.pick(obj,x) : _.pick(obj,y);
    });
};

LeapWrapperForImpress.prototype.controlByClosure = function(param,frame){
    var stack = [];
//  var gestureKind = ["circle","swipe","screenTap","keyTap"];

    if (stack.length<param) {
        var types = _(frame.gestures).map(function(x){return x.type;});
        var type  = Hiyoko.prototype.getModeOf(types);
        return stack.concat(type);
    } else {
        return function(){
            return Hiyoko.prototype.getModeOf(stack);
        };
    }
};

// using global variable
var stack=[];
function callAPIorPush(){
    if (stack.length>param) {
        nextPage();
        stack=[];
    } else {
        stack.push(gesture);
    }
};
whileLoop {
    callAPIorPush();
}

// using closure
// Array -> Array -> Array
    // 非破壊的操作だけで書けるのか・・・?
    // 副作用がなければいいという話でもあるが、

function controler(gestures){
    var now = stacker(past, gestures);
    if (now == []) {
        // pageControll
    }
}

// stacker
// gestures -> {flag, gestures}
// 内部状態を記憶

// selecter
// {flag, gestures} -> gesture

// controller
// gesture -> IO page control

var now = stacker(gestures);
var gesture = selecter(now);
controller(gesture);


function stacker(gestures){
    var stack;

    return {
        add: function(array){
            stack.push(array);
        },
        getMode: function(array){
            return modeOf(array);
        },
        control: function(value){
            // control
        },
        init: function(){
            stack = [];
            return;
        }
    }
};

whileLoop {
    var gestures = //
    hom.add(gesture);
    if (hom.getGesture)
        hom.select();
        hom.control();
        hom.init();
}

var gestureStacked(stack,param,frame);

LeapWrapperForImpress.prototype.controlByPromise = function(param,frame){
    var d = $.Deferred();
    
    $.when("")
        .then(function(){
            setTimeout(function(){
                // process;
            },param/3)
        })
        .then(function(){
            setTimeout(function(){
                // process;
            })
        })
        .then(function(){
            setTimeout(function(){
                d.resolve("done");
            }, param)
        })
    
    return d.promise();
};

var yearning = controlByPromise().done();
if (yearning.state() == "resolved") {
    switch (gesture.type) {
        case 'circle':
            this.gotoOverview();
            break;
        case 'swipe':
            if (gesture.direction[0] > 0) {
                this.gotoNext();
                break;
            } else {
                this.gotoPrevious();
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

function go(){
    var d = $.Deferred();
    
    $.when("")
        .then(function(){
            setTimeout(function(){
                console.log("sub-task 1");
            }, 5000)
        })
        .then(function(){
            setTimeout(function(){
                console.log("sub-task 2");
            }, 10000)
        })
        .then(function(){
            setTimeout(function(){
                d.resolve("done done");
            }, 15000)
        })
    
    return d.promise();
}

LeapWrapperForImpress.prototype.getAttributes = function(tagName,attribute){
    return document.getElementsByTagName(tagName)
        .map(function(elem){return elem.getAttribute(attribute);})
        .filter(function(attr){return attr != null;});
};

LeapWrapperForImpress.prototype.getErea = function(){
    var xs = this.getAttributes('div',"data-x")
    var ys = this.getAttributes('div',"data-y")
    return {"left":xs.min(),"top":ys.max(),"right":xs.max(),"bottom":ys.min()};
};

LeapWrapperForImpress.prototype.dispatchKeyboardEvent = function(type,keyCode){
    // Create a keyboard event
    var keyEvent = document.createEvent("Events");
    
    // Initialize the keyboard event
    keyEvent.initEvent(type,true,true);
    keyEvent.keyCode = keyCode;
    
    // Dispatch the keyboar event
    document.dispatchEvent(keyEvent);
    return keyEvent;
};



// ---------------------------------------------------
// Carrier

// function __triggerKeyboardEvent(el, keyCode)
// {
//     var eventObj = document.createEventObject ? document.createEventObject() : document.createEvent("Events");
//     
//     if(eventObj.initEvent){
//       eventObj.initEvent("keydown", true, true);
//     }
//     eventObj.keyCode = keyCode;
//     eventObj.which = keyCode;
//     
//     el.dispatchEvent ? el.dispatchEvent(eventObj) : el.fireEvent("onkeydown", eventObj);
// }
// 
// function triggerKeyboardEvent(el, keyCode){
//     var keyboardEvent = document.createEvent("KeyboardEvent");
//     
//     var initMethod = typeof keyboardEvent.initKeyboardEvent !== 'undefined' ? "initKeyboardEvent" : "initKeyEvent";
//     keyboardEvent[initMethod](
//                        "keydown",
//                         true,      // bubbles oOooOOo0
//                         true,      // cancelable
//                         window,    // view
//                         false,     // ctrlKeyArg
//                         false,     // altKeyArg
//                         false,     // shiftKeyArg
//                         false,     // metaKeyArg
//                         keyCode,   // keyCode
//                         0          // charCode
//     );
//     
//     el.dispatchEvent(keyboardEvent);
// }

//  // jQuery Version
//  LeapWrapperForImpress.prototype.triggerKeyboardEvent = function(type,keyCode){
//      var keyEvent = $.Event(type,{keyCode: keyCode});
//      $(document).trigger(keyEvent);
//      return keyEvent;
//  }

//        // ref
//        var keyEvent = document.createEventObject();
//        keyEvent.keyCode = ;
//        document.body.fireEvent("onkeydown",obj);
//        var _e = document.createEvent("KeyboardEvent");
//        _e.initKeyEvent("keydown",true,true,null,false,false,false,false,65,0);
//        document.getElementById("").dispatchEvent(_e);

//          var keyEvent = document.createEvent("KeyboardEvent");
//          keyEvent.keyCode = 32;
//          console.log(keyEvent);
//          keyEvent.initEvent("keydown",true,false);
//          console.log(keyEvent.keyCode + "!!")
//          document.getElementById("impress").dispatchEvent(keyEvent);

//      // Create a keyboard event
//      var keyEvent = document.createEvent("KeyboardEvent")

//      // Initialize the keyboard event (chrome)
//      keyEvent.initKeyboardEvent(
//          "keyup", //type,
//          true,    //canBubble,
//          true,    //cancelable,
//          window,    //view,
//          keyCode, //keyIdentifier,
//          0,       //keyLocation,
//          false,   //ctrlKey,
//          false,   //shiftKey,
//          false,   //altKey,
//          false,   //metaKey,
//          false    //altGraphKey
//         );

//      // Initialize the keyboard event (Firefox)
//      keyEvent.initKeyEvent(
//          type,    //  in DOMString typeArg,
//          true,    //  in boolean canBubbleArg,
//          true,    //  in boolean cancelableArg,
//          null,    //  in nsIDOMAbstractView viewArg, Specifies UIEvent.view. This value may be null.
//          false,   //  in boolean ctrlKeyArg,
//          false,   //  in boolean altKeyArg,
//          false,   //  in boolean shiftKeyArg,
//          false,   //  in boolean metaKeyArg,
//          keyCode, //  in unsigned long keyCodeArg,
//          0);      //  in unsigned long charCodeArg);
