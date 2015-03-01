$(function(){
  bullet = $.bullet('ws://localhost:8080/gridlock');
  bullet.onopen = function(){
      console.log('bullet: opened');
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

  game_created : function(data){
    console.log('game created', data);
    if($('#new-grid-name').val() == data.name){
      $('#newGridModal').modal('hide');
      bullet.send(JSON.stringify({ event: "draw_game", name: data.name}));
    }
    var link = $('<li><a href="#'+data.name+'">'+data.name+'</a></li>');
    link.on('click', function(){ console.log("clicked :", data.name); });
    $('#join-grid-list').append(link);
  },

  draw_game : function(data){
    console.log("Drawing grid: ", data);
    var box = $('#grid-box'),
        table = $('<table></table>');
    box.empty();

    for(i=1; i<=data.size; i++){
      var row = $('<tr></tr>');
      for(j=1; j <= data.size; j++){
        var cell = $('<td class="x'+i+' y'+j+' square">?</td>');
        row.append(cell);
      }
      table.append(row)
    }
    box.append(table);
  }
};
