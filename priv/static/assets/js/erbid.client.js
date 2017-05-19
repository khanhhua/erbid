import PlaceBidModal from './clients/place-bid-modal.jsx';
import reactDOM from 'react-dom';

const erbid = {
  // init,
  scan,
  apiKey: undefined
};

const state = {
  productListings:[]
};

window.erbid = window.erbid || erbid;
window.erbidCallback = function () {};

init(window.ERBID_APIKEY);

function init(apiKey) {
  if (!apiKey) {
    throw new Error('API key missing');
  }

  console.info('[init] Initializing')
  erbid.apiKey = apiKey;

  window.addEventListener('load', function () {
    scan();
    validate();

    if (typeof(erbidCallback) === 'function') {
      erbidCallback();
    }

    const erbidNodes = document.querySelectorAll('[erbid]');
    Array.prototype.forEach.call(erbidNodes, node => {
      const button = node.querySelector('[erbid-button-bid]');
      if (!button) {
        return;
      }
      button.addEventListener('click', showPlaceBidModal);
    });
  });
}

function scan() {
  const productListings = extractProductListing(document.querySelectorAll('[erbid]'));

  console.info('[scan] Product listings:', productListings);
  state.productListings = productListings;
}

function validate() {
}

function showPlaceBidModal () {
  const node = document.body.appendChild(document.createElement('div'));
  reactDOM.render(<PlaceBidModal />, node);
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
