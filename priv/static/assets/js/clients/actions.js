export const placeBid = (productId, bidValue) => (dispatch, getState) => {
  console.log('[placeBid] Placing bid against: %s %s', productId, bidValue);

  dispatch({
    type: 'place-bid',
    status: 'PENDING'
  });

  setTimeout(() => {
    console.log('[placeBid] Done');

    dispatch({
      type: 'place-bid',
      status: 'SUCCESS',
      payload: {
        ok: true
      }
    });
  }, 5000);
};
