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

var toStringList = function(objList) {
  return objList.map(function(obj) { return obj.name; });
};

angular.module('iconApp')
  .controller('MainCtrl', function ($scope, IconNames , DrawIcon, $log) {
    IconNames.query({},function(data){
      $scope.iconList = toStringList(data);
      $scope.iconName = 'overview';
    });
    $scope.shadow = true;
    $scope.onBackground = true;
    $scope.mainColor = '00FF00';
    $scope.bgColor = 'F0FF00';
    $scope.lineColor = 'FFFF00';
    $scope.iconUrl = '';
    $scope.updateIcon = function() {
      $log.info('Changed something');
    };
    $scope.$watch(combinedWatch, function() {
      $log.info('Watching works!');
      //
      DrawIcon.get(iconParams($scope), function(data) {
        $log.info('Got success' + data.url);
        $scope.iconUrl = data.url;
      });
    });
  });
