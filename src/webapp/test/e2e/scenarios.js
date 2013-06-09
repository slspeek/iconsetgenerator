/* http://docs.angularjs.org/guide/dev_guide.e2e-testing */

describe('Iconset Generator ', function() {

    beforeEach(function() {
        browser().navigateTo('/app/');
      });


    it(
      'should automatically redirect to a valid url', function() {
        expect(browser().location().url()).toBe(
          '/overview/0077FD/F011DD/A20099/true/true');
      });

    describe('shadow checkbox', function() {

        it('should render view1 when user navigates to /view1', function() {
            input('shadow').check(false);
            element('#save').click();
            expect(browser().location().url()).toBe(
              '/overview/0077FD/F011DD/A20099/false/true');
          });

      };

      describe('colorpicker', function() {

          it(
            'open a colorpicker, change the color to 123456, and verify it worked', function() {
              element('#mainColor').click();
              input('ui-colorpicker-hex-input').enter('123456');
              //The magic with the keyup event is needed to trigger the colorpicker
              //seeing a change.
              element('input.ui-colorpicker-hex-input').query(function(el, done) {
                  var evt = document.createEvent('Event');
                  evt.initEvent('keyup', false, true);
                  el[0].dispatchEvent(evt);
                  done();
                });
              element('button.ui-colorpicker-ok').click();
              expect(element('#mainColor').val()).toBe('123456');
            });

        });

    });
