'use strict';

$( function() {
        var doRequest = function() {
                var lineColor = $('#linecolor').val();
                var mainColor = $('#maincolor').val();
                var bgColor = $('#background').val();
                var iconName = $('#icon').val();
                var onBackground = $('#onbackground').is(':checked');
                var shadow = $('#shadow').is(':checked');
                var url = '/icongenerator?linecolor=' + lineColor +
                        '&maincolor=' + mainColor +
                        '&bgcolor=' + bgColor +
                        '&iconname=' + iconName +
                        '&width=500&height=500';
                if (onBackground) {
                  url = url + '&onbackground=true';
                }
                if (shadow) {
                  url = url + '&shadow=true';
                }
                console.log('URL: ' + url);
                $.getJSON(url, function(data) {
                                        console.log('data: ' + data);
                                        $('#icon_display').attr('src', data);

                                      }).fail(function() {
                                        console.log('Failed');
                                      });
              };
        var fillIconList = function(data) {
                var options = '';
                for (var i = 0; i < data.length; i++) {
                  var item = data[i];
                  options += '<option value="' + item + '">' + item + '</option>';
                }
                $('select#icon').html(options);
                doRequest();
              };
        var loadIconNames = function() {
                $.getJSON('/iconlist', fillIconList);
              };
        $('.cp-input').colorpicker({
          close: function() {
                  doRequest();
                }
        });
        $('.icon-input').on('change',
                        function() {
                          doRequest();
                        });
        loadIconNames();
      });
