export default class PlaceBidModal extends React.Component {
  render() {
    const {placeBid, closeModal} = this.props.actions;
    const {listing} = this.props;

    return (
      <div className="modal fade in" style={{display:'block'}} tabIndex="-1" role="dialog">
        <div className="modal-dialog" role="document">
          <div className="modal-content">
            <div className="modal-header">
              <button type="button" className="close" aria-label="Close"
                      onClick={closeModal}>
                <span aria-hidden="true">&times;</span>
              </button>
              <h4 className="modal-title">Place a Bid</h4>
            </div>
            <div className="modal-body">
              <div className="container-fluid">
                <div className="row">
                  <div className="col-xs-6 col-sm-4">
                    <div className="thumbnail">
                      <img src={listing.image_url} title={listing.title} />
                    </div>
                  </div>
                  <div className="col-xs-6 col-sm-8">
                    <dl className="dl-horizontal">
                      <dt>Title:</dt><dd>{listing.title}</dd>
                      <dt>Description:</dt><dd>{listing.description}</dd>
                      <dt>Deadline:</dt><dd>{listing.deadline}</dd>
                    </dl>
                  </div>
                </div>
                <div className="row">
                  <div className="col-xs-12 col-sm-10 form-horizontal">
                    <div className="form-group">
                      <label className="control-label col-xs-12 col-sm-4">Winning bid</label>
                      <div className="col-xs-12 col-sm-6">
                        <div className="input-group">
                          <span className="input-group-addon">USD</span>
                          <span className="form-control text-right">{listing.price}</span>
                        </div>
                      </div>
                    </div>
                    <div className="form-group">
                      <label className="control-label col-xs-12 col-sm-4">Your bid</label>
                      <div className="col-xs-12 col-sm-6">
                        <div className="input-group">
                          <span className="input-group-addon">USD</span>
                          <input className="form-control text-right"
                                 type="number"
                                 placeholder={listing.price}
                                 min={listing.price}
                                 ref="bidValue" />
                          <div className="input-group-btn">
                            <button className="btn btn-success"
                                    style={{position: 'absolute', top: 0}}
                                    onClick={() => placeBid(listing.id, this.refs.bidValue.value)}>
                              PLACE BID
                            </button>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div className="modal-footer">
              <button type="button" className="btn btn-default"
                      onClick={closeModal}>
                Close
              </button>
            </div>
          </div>
        </div>
      </div>
    );
  }
};
