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
  productListings:[],
  elementMapping: {} // Maps of listing IDs to Latest Bid Output Elements
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
    // Extract erbid specific elements
    let erbidElements = scan();

    const pids = erbidElements.map(item => item.pid);
    validate(state);

    getListings(pids).then(function(listings) {
      if (typeof(erbidCallback) === 'function') {
        erbidCallback();
      }

      console.info('[init]', listings);
      state.productListings = listings;

      listings.forEach(listing => {
        console.info('[init] Processing UI elements for %s', listing.id);
        const erbidElement = erbidElements.find(item => item.pid === listing.id);

        if (!erbidElement) {
          return;
        }

        wireListingToElement(listing, erbidElement);
      });

      subscribeToErbidServer();
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

function wireListingToElement(listing, element) {
  const button = element.elementButtonBid;
  const latestBid = element.elementLatestBid;

  if (button) {
    console.info('[wireListingToElement] Wiring bid button (pid: %s) to event handler', listing.id);
    button.addEventListener('click', (e) => showPlaceBidModal(listing.id));
  }

  if (latestBid) {
    console.info('[wireListingToElement] Channeling latest bid (pid: %s) to output element', listing.id);

    state.elementMapping[listing.id] = latestBid;
  }
}


function validate() {
}

function showPlaceBidModal (productId) {
  const node = document.body.appendChild(document.createElement('div'));

  const localState = {
    listing: state.productListings.find(item => item.id === productId),
    modal: { visible: true }
  };

  if (!localState.listing) {
    console.warn('Page contains no valid listings');
    return;
  }

  let store = compose(applyMiddleware(thunk))(createStore)(
    (state=localState, action) => {
      switch (action.type) {
        case 'place-bid':
        case 'update-listing':
          const {listing} = action.payload;
          if (listing.id !== state.listing.id) {
            return state;
          }

          return Object.assign({}, state, {listing});
        case 'close-modal':
          return Object.assign(
            {},
            state,
            {
              modal: { visible: false }
            });
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

  const app = (<Provider store={store}>
    <ConnectedPlaceBidModal />
  </Provider>);

  reactDOM.render(app, node);
  store.subscribe((e) => {
    const {modal: {visible}} = store.getState();
    if (!visible) {
      node.remove();
    }
  })
}

function subscribeToErbidServer() {
  // Faker
  const handler = setInterval((function generate () {
    console.info('[subscribeToErbidServer] Generating...');
    const {productListings, elementMapping} = state;

    state.productListings = randomize(state.productListings);

    if (erbid.__dispatch) {
      state.productListings.forEach(listing => {
        erbid.__dispatch({
          type: 'update-listing',
          payload: {
            listing
          }
        })
      });
    }
    state.productListings.forEach(listing => {
      const latestBid = elementMapping[listing.id];
      latestBid.innerText = '$' + listing.price;
    });

    return generate;
  })(),  5000);

  return {
    unsubscribe () {
      clearInterval(handler);
    }
  };

  function randomize (productListings) {
    const updatedListings = productListings.map(listing => Object.assign({}, listing));

    updatedListings.forEach(listing => {
      listing.price += Math.floor(Math.random() * 20)
    });

    return updatedListings;
  }
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
      pid: node.getAttribute('erbid-product-id'),
      elementLatestBid: node.querySelector('[erbid-product-latest-bid]'),
      elementButtonBid: node.querySelector('[erbid-button-bid]')
    };
  }
}
