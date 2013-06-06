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

var updateLocation = function($scope, $location) {
  $location.url(iconHistoryToken($scope));
};

var strToBoolean = function(str) {
  if (str === 'false') {
    return false;
  } else {
    return true;
  }
};
angular.module('iconApp')
  .controller('MainCtrl', function($scope, $routeParams, $location, IconNames,
    DrawIcon, $log) {
    $scope.iconList = IconNames.list();
    $scope.shadow = strToBoolean($routeParams.Shadow);
    $scope.onBackground = strToBoolean($routeParams.Background);
    $scope.mainColor = $routeParams.MainColor;
    $scope.bgColor = $routeParams.BgColor;
    $scope.lineColor = $routeParams.LineColor;
    $scope.iconName = $routeParams.IconName;
    $scope.iconUrl = '';
    $scope.pressS = function() {
      $log.info('Changed something');
    };
    $scope.saveLocation = function() {
      $log.info('Saving location');
      updateLocation($scope, $location);
    };
    $scope.$watch(combinedWatch, function() {
        $log.info('Watching works!');
        $scope.iconUrl = DrawIcon.get(iconParams($scope));
      });
    $scope.setLineEqualToMainColor = function() {
      $scope.lineColor = $scope.mainColor;
      $log.info('line: ' + $scope.lineColor + ' main: ' + $scope.mainColor);
    };
  });
