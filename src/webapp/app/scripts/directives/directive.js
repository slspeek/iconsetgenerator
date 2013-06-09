'use strict';

/* Directives */
var app = angular.module('iconApp');

// Add a directive for the bootstrap color picker widget
// http://www.eyecon.ro/bootstrap-colorpicker/
app.directive('colorpicker', function() {

    return {
      require: '^ngModel',
      scope: {
        cpOnClick: '&cpOnClick',
        model: '=ngModel'
      },
      link: function(scope, element, attrs, controller) {
        var updateModel;
        if (!(controller === null || controller === undefined)) {
          updateModel = function(value) {
            return scope.$apply(function() {
                scope.model = value;
              });
          };
          controller.$render = function() {
            element.val(scope.model);
            return element.colorpicker({
                select: function(e, color) {
                  if (updateModel) {
                    updateModel(color.formatted);
                  }
                },
                close: function() {
                  scope.$apply(function() {
                      scope.cpOnClick();
                    });
                },
                inline: true
              });
          };
        }

        return element.colorpicker({
            color: controller.$viewValue
          }).on('changeColor', function(e) {
            if (updateModel) {
              updateModel(e.color.toHex());
            }
          });

      }
    };
  });
