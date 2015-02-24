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
      console.log(e.data);
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
