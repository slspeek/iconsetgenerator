'use strict';

describe('colorpicker', function() {
  var elm, scope;

  // load the tabs code
  beforeEach(module('iconApp'));


  beforeEach(inject(function($rootScope, $compile) {
    // we might move this tpl into an html file as well...
    elm = angular.element(
      '<div>' + 
        '<div id="color">{{color}}</div>' +
        '<input cp-on-click="alert()" id="colorpicker" ng-model="color" colorpicker ></input>'+
      '</div>'
      );
    
    scope = $rootScope;
    $compile(elm)(scope);
    scope.$digest();
  }));


  it('should compile ', function() {
    var input = elm.find('#colorpicker');
    var color = elm.find('#color');
    //jQuery(input).click();
    expect(scope.color).toBeUndefined();
    scope.$apply(function() {
      scope.color = 'AABBCC';
      });
    expect(scope.color).toBe('AABBCC');
//    expect(input.val()).toBe('AABBCC');
//    expect(color.text()).toBe('AABBCC');
  });



});
