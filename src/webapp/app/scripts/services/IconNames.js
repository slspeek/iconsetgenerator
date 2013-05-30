'use strict';

angular.module('iconApp')
  .factory('IconNames', function ($resource) {
        return $resource('/iconlist');
      });

