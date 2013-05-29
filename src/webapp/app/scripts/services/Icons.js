'use strict';

angular.module('iconApp')
  .factory('Icons', function ($resource) {
        return $resource('/iconlist');
      });

