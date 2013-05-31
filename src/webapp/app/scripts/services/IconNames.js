'use strict';

var toStringList = function(objList) {
  return objList.data.map(function(obj) { return obj.name; });
};

angular.module('iconApp')
  .factory('IconNames', function ($http) {
        return {
          list: function(success) {
            return $http.get('/iconlist').
              then(toStringList).
              then(success);
          }
        };
      });

