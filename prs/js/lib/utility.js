// ----------
// Class
// ----------

var Utility = (function(){
    // this => window

    // Constructor
    function Utility(){
        // this => Utility
    }
    
    return Utility;
})();

// ----------
// Method
// ----------

Utility.prototype.min = function(){
    return this.reduce(function(x,y){return (x<y)?x:y})
};

Utility.prototype.max = function(){
    return this.reduce(function(x,y){return (x>y)?x:y})
};
