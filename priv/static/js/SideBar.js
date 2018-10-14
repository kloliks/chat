class SideBarBody extends React.Component {
  render () {
    const channel = this.props.channel
    const name = this.props.name
    const unread = channel.unread && <span className="unread-meta pull-right">{channel.unread}</span> || ''
    const onclick = this.props.onclick
    return (
      <div className="sideBar-body" onClick={onclick}>
        <span className="sideBar-avatar"><i className="fab fa-weixin fa-2x"></i></span>
        <span className="name-meta">{name}</span>
        {unread}
      </div>
    )
  }
}

class SideBarBodyList extends React.Component {
  render () {
    const channels = this.props.channels
    const onclick = this.props.onclick
    const r = channels.map(function (channel) {
      const name = channel.alias || channel.name
      return (
        <SideBarBody key={name} channel={channel} name={name} onclick={onclick(channel.name)} />
      )
    })
    return (
      r
    )
  }
}
