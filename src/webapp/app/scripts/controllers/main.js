'use strict';

angular.module('iconApp')
  .controller('MainCtrl', function ($scope, Icons , $log) {
    $scope.iconList = Icons.query();
    $scope.shadow = true;
    $scope.onBackgroud = true;
    $scope.mainColor = '00FF00';
    $scope.bgColor = '00FF00';
    $scope.lineColor = '00FF00';
    $scope.iconUrl = '';
    $scope.updateIcon = function() {
      $log.info('Changed something');
    };
    $scope.$watch('bgColor', function() {
      $log.info('Watching works!' + arguments);
    });
  });
