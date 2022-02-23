// var ace = false;
var GlobalTester = (function(){
  var fields = {};
  var before = function(w){
      for(var field in w){
          fields[field] = true;
      };
  };

  var after = function(w){
      for(var field in w){
          if(!fields[field]){
               console.log(field + " has been added");
          }
      };

  };
  return {before: before, after:after};
}());

GlobalTester.before(window);
