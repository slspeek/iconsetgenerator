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
  var params =
    {
      width: 400,
      height: 400,
      maincolor: $scope.mainColor,
      bgcolor: $scope.bgColor,
      linecolor: $scope.lineColor,
      iconname: $.trim($scope.iconName)
    };
  if ($scope.shadow) {
    params.shadow = true;
  }
  if ($scope.onBackground) {
    params.onbackground = true;
  }
  return params;
};

var isTrue = function(arg) {
  if (arg) {
    return true;
  } else {
  	return false;
  }
}
angular.module('iconApp')
  .controller('MainCtrl', function ($scope, $routeParams, $location, IconNames , DrawIcon, $log) {
    $scope.iconList = IconNames.list();
    $scope.shadow = isTrue($routeParams.Shadow);
    $scope.onBackground = isTrue($routeParams.Background);
    $scope.mainColor = $routeParams.MainColor;
    $scope.bgColor = $routeParams.BgColor;
    $scope.lineColor = $routeParams.LineColor;
	$scope.iconName = $routeParams.IconName;
    $scope.iconUrl = '';
    $scope.pressS = function() {
      $log.info('Changed something');
    };
    $scope.$watch(combinedWatch, function() {
      $log.info('Watching works!');
      $scope.iconUrl = DrawIcon.get(iconParams($scope));
		if ($scope.mainColor !== undefined && $scope.bgColor !== undefined &&
			$scope.lineColor !== undefined) {
			$location.url('designer/' + $scope.iconName + '/' +
						  $scope.mainColor + '/' +
						  $scope.bgColor + '/' +
						  $scope.lineColor + '/' +
						  $scope.shadow + '/' +
						  $scope.onBackground);
	    }
    });
    $scope.setLineEqualToMainColor = function() {
      $scope.lineColor = $scope.mainColor;
      $log.info('line: ' + $scope.lineColor + ' main: ' + $scope.mainColor);
    };
  });
