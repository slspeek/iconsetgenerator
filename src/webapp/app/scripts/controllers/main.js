'use strict';

angular.module('iconApp')
  .controller('MainCtrl', function ($scope, $http) {
    $http.get('/iconlist').success(function(data) {
      $scope.iconList = data;
    });
    $scope.shadow = true;
    $scope.mainColor = "00FF00";
  });
