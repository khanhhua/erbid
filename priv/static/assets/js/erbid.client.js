import PlaceBidModal from './clients/place-bid-modal.jsx';
import reactDOM from 'react-dom';

import { connect } from 'react-redux';
import { createStore, bindActionCreators, applyMiddleware, compose } from 'redux';
import { Provider } from 'react-redux';
import thunk from 'redux-thunk';

import * as actions from './clients/actions';

const erbid = {
  // init,
  scan,
  apiKey: undefined
};

let state = {
  productListings:[]
};

window.erbid = window.erbid || erbid;
window.erbidCallback = function () {};

init(window.ERBID_APIKEY);

function init(apiKey) {
  if (!apiKey) {
    throw new Error('API key missing');
  }

  console.info('[init] Initializing...');
  erbid.apiKey = apiKey;

  window.addEventListener('load', function () {
    let listings = scan();

    const pids = listings.map(item => item.pid);
    validate(state);

    getListings(pids).then(function(listings) {
      if (typeof(erbidCallback) === 'function') {
        erbidCallback();
      }

      console.info('[init]', listings);
      state.productListings = listings;

      const erbidNodes = document.querySelectorAll('[erbid]');
      Array.prototype.forEach.call(erbidNodes, node => {
        const button = node.querySelector('[erbid-button-bid]');
        if (!button) {
          return;
        }
        button.addEventListener('click', (e) => showPlaceBidModal('w1kv'));
      });

      console.info('[init] Complete');
    });
  });
}

function scan() {
  const productListings = extractProductListing(document.querySelectorAll('[erbid]'));

  console.info('[scan] Product listings:', productListings);

  return productListings;
}

function getListings(pids) {
  return fetch('/api/resources/listings/').then(res => res.json()).then(data => {
    return data.listings;
  });
}

function validate() {
}

function showPlaceBidModal (productId) {
  const node = document.body.appendChild(document.createElement('div'));

  const localState = {
    listing: state.productListings.find(item => item.id === productId)
  };

  if (!localState.listing) {
    console.warn('Page contains no valid listings');
    return;
  }

  let store = compose(applyMiddleware(thunk))(createStore)(
    (state=localState, action) => {
      switch (action.type) {
        case 'place-bid': return state;
        case 'update-listing':
          const {listing} = action.payload;
          if (listing.id !== state.listing.id) {
            return state;
          }

          return {listing};
      }

      return state;
    }
  );

  erbid.__dispatch = store.dispatch;

  const ConnectedPlaceBidModal = connect(
    function mapStateToProps (state) {
      const {listing} = state;

      return {
        listing
      };
    },
    function mapDispatchToProps (dispatch, ownProps) {
      return {
        actions: bindActionCreators(actions, dispatch)
      };
    }
  )(PlaceBidModal);

  const app = (<Provider store={store}><ConnectedPlaceBidModal /></Provider>);

  reactDOM.render(app, node);
}

//============================================================
// PRIVATE SECTION
//============================================================
/**
 *
 **/
function extractProductListing(nodeList) {
  const data = Array.prototype.slice.call(nodeList).map(extract);

  return data;

  function extract(node) {
    return {
      pid: node.getAttribute('erbid-product-id')
    };
  }
}
