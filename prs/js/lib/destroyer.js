// ----------
// Class
// ----------

var Destroyer = (function(){
    // this => window

    // Constructor
    function Destroyer(){
        // this => Destroyer
    }
    
    return Destroyer;
})();

// ----------
// Method
// ----------

Destroyer.prototype.attachMethodsToClass = function(fromClasses,toClass){
    fromClasses.forEach(function(fromClass){
        var methods = Object.getOwnPropertyNames(fromClass.prototype);
        methods.forEach(function(method){
            toClass.prototype[method] = function(){
                return fromClass.prototype[method].apply(this,arguments);
            };
        });
    });
};
