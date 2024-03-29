/*globals strToBoolean, initScope, expect, inject, module, it, beforeEach, describe */
(function() {
  'use strict';

  var IconNamesMock = {
    list: function() {
      return ['play', 'stop'];
    }
  };

  describe('Controller: IconDesignerCtrl', function() {

      // load the controller's module
      beforeEach(module('iconApp'));

      var IconDesignerCtrl,  scope;

      // Initialize the controller and a mock scope
      beforeEach(inject(function($controller, $rootScope) {
            scope = $rootScope.$new();
            IconDesignerCtrl = $controller('IconDesignerCtrl', {
                $scope: scope,
                IconNames: IconNamesMock,
                $routeParams: {
                  BgColor: '000000',
                  MainColor: '110011',
                  LineColor: '0022DD',
                  Shadow: 'False',
                  IconName: 'MoneyLisa'
                }
              });
          }));

      it('should set bgColor', function() {
          expect(scope.bgColor).not.toBeUndefined();
        });

      it('should set iconName', function() {
          expect(scope.iconName).toBe('MoneyLisa');
        });
      it('should set mainColor', function() {
          expect(scope.mainColor).toBe('110011');
        });
      it('should set lineColor', function() {
          expect(scope.lineColor).toBe('0022DD');
        });
      it('should set shadow boolean', function() {
          expect(scope.shadow).toBe(true);
        });
      it('shoud set iconlist', function() {
          expect(scope.iconList).toEqual(['play', 'stop']);
        });
    });

  describe('strToBoolean', function() {
      it('should return false on the string false', function() {
          expect(strToBoolean('false')).toBe(false);
        });
      it('should return true on the string true', function() {
          expect(strToBoolean('true')).toBe(true);
        });
    });

  describe('initScope', function() {
    it('should default to true for shadow and onBackground', function() {
      var scope = {};
      initScope({}, scope);
      expect(scope.shadow).toBe(true);
      expect(scope.onBackground).toBe(true);
    });
    it('should give false for shadow and onBackground', function() {
      var scope = {};
      initScope({Shadow:'false', Background:'false'}, scope);
      expect(scope.shadow).toBe(false);
      expect(scope.onBackground).toBe(false);
    });
    it('should copy IconName param', function() {
      var scope = {};
      initScope({IconName:'icon'}, scope);
      expect(scope.iconName).toBe('icon');
    });
    it('should copy MainColor param', function() {
      var scope = {};
      initScope({MainColor:'icon'}, scope);
      expect(scope.mainColor).toBe('icon');
    });
  });
}());
