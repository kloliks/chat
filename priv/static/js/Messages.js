function hashCode(str) {
    var hash = 0;
    for (var i = 0; i < str.length; i++) {
       hash = str.charCodeAt(i) + ((hash << 5) - hash);
    }
    return hash;
}

function intToRGB(i){
    var c = (i & 0x00FFFFFF)
        .toString(16)
        .toUpperCase();

    return "00000".substring(0, 6 - c.length) + c;
}

class MessageBody extends React.Component {
  render () {
    const message = this.props.message
    var date = new Date()
    date.setTime(message.time / 1000000)
    const time = date.toString()
    const name = message.nickname
    const text = message.text.slice().split('\n').map(function (line, index) {
      return (
        <div key={index}>{line}</div>
      )
    })
    const ncolor = "#" + intToRGB(hashCode(name))
    return (
      <div className="message-body">
        <div>
          <span style={{color: ncolor}} className='message-name'>{name}</span>
          <span className='message-time'>{time}</span>
        </div>
        <div>{text}</div>
      </div>
    )
  }
}

class MessageBodyList extends React.Component {
  render () {
    const messages = this.props.messages.slice().reverse()
    const r = messages.map(function (message) {
      return (
        <MessageBody key={message.time} message={message} />
      )
    })
    return (
      r
    )
  }
}
