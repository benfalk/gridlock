$(function(){
  bullet = $.bullet('ws://'+window.location.hostname+':'+window.location.port+'/gridlock');
  bullet.onopen = function(){
      console.log('bullet: opened');
      bullet.send(JSON.stringify({ event: "game_list" }));
  };
  bullet.ondisconnect = function(){
      console.log('bullet: disconnected');
  };
  bullet.onclose = function(){
      console.log('bullet: closed');
  };
  bullet.onmessage = function(e){
      if(e.data == "pong"){ return; }
      var data = JSON.parse(e.data);
      if(data.event && grid_handler[data.event]){
        grid_handler[data.event](data);
      }
      else{
        console.log("Unhandled msg: ", data);
      }
  };
  bullet.onheartbeat = function(){
      bullet.send('ping');
  }

  $('#create-new-grid').click(function(){
    var build_event = JSON.stringify({
      event: "create_game",
      name: $('#new-grid-name').val(),
      size: parseInt($('#new-grid-size').val())
    });
    bullet.send(build_event)
  });
});

var grid_handler = {
  current_grid : '',

  game_list : function(data) {
    console.log("Populating games", data);
    data.games.forEach(function(game){
      var link = $('<li><a href="#'+game+'">'+game+'</a></li>');
      link.on('click', function(){
        bullet.send(JSON.stringify({ event: "draw_game", name: game}));
      });
      $('#join-grid-list').append(link);
    });
  },

  game_created : function(data){
    console.log('game created', data);
    if($('#new-grid-name').val() == data.name){
      $('#newGridModal').modal('hide');
      bullet.send(JSON.stringify({ event: "draw_game", name: data.name}));
    }
    var link = $('<li><a href="#'+data.name+'">'+data.name+'</a></li>');
    link.on('click', function(){
      bullet.send(JSON.stringify({ event: "draw_game", name: data.name}));
    });
    $('#join-grid-list').append(link);
  },

  draw_game : function(data){
    console.log("Drawing grid: ", data);
    var box     = $('#grid-box'),
        table   = $('<table></table>'),
        grid_pt = 0;

    box.empty();
    this.current_grid = data;

    for(i=1; i<=data.size; i++){
      var row = $('<tr></tr>');
      for(j=1; j <= data.size; j++){
        var status = data.grid[grid_pt].status;
        var bombs = data.grid[grid_pt].surrounding_bombs;
        var has_bomb = data.grid[grid_pt].has_bomb;
        var b_txt = has_bomb ? 'X' : bombs > 0 ? bombs : '';
        var cell = $('<td class="x'+i+' y'+j+' square status-'+status+' bombs-'+bombs+'">'+b_txt+'</td>');
        cell.data('square', data.grid[grid_pt]);
        cell.on('click', function(){
          var datum = $(this).data('square');
          bullet.send(JSON.stringify({
            event: 'uncover_square',
            location: datum.location,
            name: grid_handler.current_grid.name
          }));
        });
        cell.on('contextmenu', function(){
          var datum = $(this).data('square');
          console.log("You right clicked", datum);
          if($(this).hasClass('status-covered')){
            bullet.send(JSON.stringify({
              event: 'flag_square',
              location: datum.location,
              name: grid_handler.current_grid.name
            }));
          }
          else if($(this).hasClass('status-flagged')){
            bullet.send(JSON.stringify({
              event: 'unflag_square',
              location: datum.location,
              name: grid_handler.current_grid.name
            }));
          }
          return false;
        });
        row.append(cell);
        grid_pt++;
      }
      table.append(row)
    }
    box.append(table);
  },

  uncover_square : function(data){
    if(data.name != this.current_grid.name){ return; }
    console.log("Going to uncover: ", data);
    var square = $($('.x'+data.location.x+'.y'+data.location.y)[0]);
    square.removeClass('status-covered');
    square.addClass('status-uncovered');
  },

  flag_square : function(data){
    if(data.name != this.current_grid.name){ return; }
    console.log("Going to flag: ", data);
    var square = $($('.x'+data.location.x+'.y'+data.location.y)[0]);
    square.removeClass('status-covered');
    square.addClass('status-flagged');
  },

  unflag_square : function(data){
    if(data.name != this.current_grid.name){ return; }
    console.log("Going to unflag: ", data);
    var square = $($('.x'+data.location.x+'.y'+data.location.y)[0]);
    square.removeClass('status-flagged');
    square.addClass('status-covered');
  }
};
