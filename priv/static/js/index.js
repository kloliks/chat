var websocket
var current_user
var maybe_user
var current_channel
var maybe_channel
var delayed_command
const server = "ws://" + window.location.host + "/websocket"

document.addEventListener("DOMContentLoaded", init)

function init() {
  document.getElementById('login').onclick = login
  document.getElementById('signup').onclick = signup
  document.getElementById('leave').onclick = leave

  joinText = document.getElementById('joinText')
  joinText.onkeydown = function (e) {
    if (e.key === "Enter" && joinText.value) {
      join(joinText.value)
    }
  }

  comment = document.getElementById('comment')
  comment.onkeydown = function (e) {
    if (e.shiftKey && e.key === "Enter" && comment.value) {
      ws_send(["SAY", comment.value])
      comment.value = null
      e.preventDefault()
    }
  }

  connect(server)
}

function connect(wsHost) {
  websocket = new WebSocket(wsHost)
  websocket.onopen = function(evt) { onOpen(evt) }
  websocket.onclose = function(evt) { onClose(evt) }
  websocket.onmessage = function(evt) { onMessage(evt) }
  websocket.onerror = function(evt) { onError(evt) }
}

function onOpen(evt) {
  current_user && login()
  current_channel && join(current_channel)
  delayed_command && delayed_command()
  delayed_command = null
}

function onClose(evt) {
}

function onMessage(evt) {
  data = JSON.parse(evt.data)
  if (data.code >= 1000) {
    if (data.code === 1006) {
      alert('Имя канала должно начинаться с "#" ("#blah") для общих каналов, или с "@" ("@nickname") для личных чатов')
    } else {
      alert(data.text)
    }
    return

  } else if (data.code === 200) {
    if (data.command === 'LOGIN') {
      document.getElementById('loginPanel').style.visibility = 'hidden'
      current_user = maybe_user

    } else if (data.command === 'SIGNUP') {
      login()

    } else if (data.command === 'JOIN') {
      current_channel = maybe_channel
      ws_send('FETCH')
    }

  } else if (data.command === 'USER_CHANGED') {
    ReactDOM.render(
      React.createElement(
        SideBarBodyList,
        { channels: data.channels,
          onclick: function (ch_name){return function (){join(ch_name)}}
        }
      ),
      document.getElementById('sideBar')
    );

  } else if (data.command === 'FETCH') {
    show_messages(data.messages)

  } else if (data.command === 'CHANNEL_CHANGED') {
    if (current_channel === data.channel) {
      ws_send('FETCH')
    }
  }
}

function onError(evt) {
  alert("websocket error: " + evt.data)
}

function ws_send(msg) {
  if(websocket.readyState == websocket.OPEN){
    websocket.send(msg)
  } else {
    connect(server)
    delayed_command = function () {ws_send(msg)}
  }
}

function login() {
  username = current_user || document.getElementById('username').value
  maybe_user = username
  ws_send(['LOGIN', username])
}

function signup() {
  username = document.getElementById('username').value
  ws_send(['SIGNUP', username])
}

function join(ch_name) {
  maybe_channel = ch_name
  ws_send(['JOIN', ch_name])
}

function show_messages(messages) {
  const conversation = document.getElementById('conversation')
  document.getElementById('heading-channel-name').innerHTML = current_channel
  ReactDOM.render(
    React.createElement(
      MessageBodyList,
      { messages: messages }
    ),
    conversation
  );
  conversation.scrollTop = conversation.scrollHeight
}

function leave() {
  current_channel = null
  show_messages([])
  ws_send('LEAVE')
}
