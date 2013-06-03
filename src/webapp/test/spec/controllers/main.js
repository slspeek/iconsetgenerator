'use strict';

describe('Controller: MainCtrl', function () {

  // load the controller's module
  beforeEach(module('iconApp'));

  var MainCtrl, $httpBackend, scope;

  beforeEach(function(){
    this.addMatchers({
    toEqualData: function(expected) {
      return angular.equals(this.actual, expected);
      }
    });
  });

  beforeEach(inject(function ($httpBackend, $rootScope) {
    scope = $rootScope.$new();
    $httpBackend.when('GET', '/iconlist').respond([{'name':'icon'}]);
  }));

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    MainCtrl = $controller('MainCtrl', {
      $scope: scope,
	  $routeParams: {BgColor: '000000',
	  				 MainColor: '110011',
					 LineColor: '0022DD',
					 Shadow: 'False',
					 IconName: 'MoneyLisa'}
    });
  }));

  it('should set bgColor', function () {
    expect(scope.bgColor).not.toBeUndefined();
  });

  it('should set iconName', function () {
    expect(scope.iconName).toBe('MoneyLisa');
  });
  it('should set mainColor', function () {
    expect(scope.mainColor).toBe('110011');
  });

  it('should set lineColor', function () {
    expect(scope.lineColor).toBe('0022DD');
  });
  it('should set shadow boolean', function () {
    expect(scope.shadow).toBe(true);
  });
});
