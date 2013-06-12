/* http://docs.angularjs.org/guide/dev_guide.e2e-testing */

describe('Colorpicker', function() {

    beforeEach(function() {
        browser().navigateTo('/app/colortest.html');
      });

    describe('colorpicker', function() {

        it(
          'open a colorpicker, change the color to 123456, and verify it worked', function() {
            element('#mainColor').click();
            //  sleep(1);
            element('input.ui-colorpicker-hex-input').val('123456');
            //              input('ui-colorpicker-hex-input').enter('123456');
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
            expect(element('#color').text()).toBe('Color: 123456');
          });

        it(
          'open a colorpicker, and verify that the color is 000000', function() {
            input('mainColor').enter('000000');
            //  sleep(1);
            element('#mainColor').click();
            expect(element('input.ui-colorpicker-hex-input').val()).toBe('000000');
            element('button.ui-colorpicker-ok').click();

          });
      });

  });
