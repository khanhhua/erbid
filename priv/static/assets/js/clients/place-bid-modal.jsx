const placeBidModal = (props) => {
  const {placeBid} = props.actions;
  const {listing} = props;

  return (
    <div className="modal fade in" style={{display:'block'}} tabIndex="-1" role="dialog">
      <div className="modal-dialog" role="document">
        <div className="modal-content">
          <div className="modal-header">
            <button type="button" className="close" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 className="modal-title">Place a Bid</h4>
          </div>
          <div className="modal-body">
            <dl className="dl-horizontal">
              <dt>ProductID:</dt><dd>{listing.id}</dd>
              <dt>Title:</dt><dd>{listing.title}</dd>
            </dl>

            <button className="btn btn-lg btn-success"
                    onClick={() => placeBid(listing.id)}>
              PLACE BID
            </button>
          </div>
          <div className="modal-footer">
            <button type="button" className="btn btn-default" data-dismiss="modal">Close</button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default placeBidModal;
