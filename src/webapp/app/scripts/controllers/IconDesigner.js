'use strict';

var combinedWatch = function($scope) {
  return $scope.onBackground +
    $scope.shadow +
    $scope.bgColor +
    $scope.mainColor +
    $scope.lineColor +
    $scope.iconName;
};

var iconParams = function($scope) {
  var params = {
    width: 400,
    height: 400,
    maincolor: $scope.mainColor,
    bgcolor: $scope.bgColor,
    linecolor: $scope.lineColor,
    iconname: $scope.iconName
  };
  if ($scope.shadow) {
    params.shadow = true;
  }
  if ($scope.onBackground) {
    params.onbackground = true;
  }
  return params;
};

var iconHistoryToken = function($scope) {
  var token = $scope.iconName + '/' +
    $scope.mainColor + '/' +
    $scope.bgColor + '/' +
    $scope.lineColor + '/' +
    $scope.shadow + '/' +
    $scope.onBackground;
  return token;
};


var strToBoolean = function(str) {
  if (str === 'false') {
    return false;
  } else {
    return true;
  }
};

var initScope = function($routeParams, $scope) {
  $scope.shadow = strToBoolean($routeParams.Shadow);
  $scope.onBackground = strToBoolean($routeParams.Background);
  $scope.mainColor = $routeParams.MainColor;
  $scope.bgColor = $routeParams.BgColor;
  $scope.lineColor = $routeParams.LineColor;
  $scope.iconName = $routeParams.IconName;
};

angular.module('iconApp')
  .controller('IconDesignerCtrl', function($scope, $routeParams, $location, IconNames,
    DrawIcon) {
    initScope($routeParams, $scope);
    $scope.iconList = IconNames.list();
    $scope.iconUrl = '';
    $scope.saveLocation = function() {
      $location.url(iconHistoryToken($scope));
    };
    $scope.$watch(combinedWatch, function() {
        $scope.iconUrl = DrawIcon.get(iconParams($scope));
      });
    $scope.setLineEqualToMainColor = function() {
      $scope.lineColor = $scope.mainColor;
    };
  });
