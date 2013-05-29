'use strict';

/* Directives */
var app = angular.module('iconApp');

// Add a directive for the bootstrap color picker widget
// http://www.eyecon.ro/bootstrap-colorpicker/
app.directive('colorpicker', function($log) {

    return {
      require: '?ngModel',
      link: function(scope, element, attrs, controller) {
        var updateModel;

        if (!(controller === null || controller === undefined)) {
          updateModel = function(value) {
            return scope.$apply(function() {
                return controller.$setViewValue(value);
              });
          };

          controller.$render = function() {
            return element.colorpicker({
                color: controller.$viewValue,
                select: function(e, color) {
                  if (updateModel) {
                    $log.info(e);
                    updateModel(color.formatted);
                  }
                }
              });
          };
        }

        return element.colorpicker({}).on('changeColor', function(e) {
            if (updateModel) {
              updateModel(e.color.toHex());
            }
          });

      }
    };
  });
