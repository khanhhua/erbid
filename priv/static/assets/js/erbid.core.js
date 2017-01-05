(function (erbidCore) {
  erbidCore.directive('modalTrigger', function () {
    return {
      restrict: 'A',
      link: function (scope, elm, attrs) {
        var modalSelector = attrs['modalTrigger'];
        if (!modalSelector) {
          return;
        }

        elm.on('click', function () {
          var $modal = $(modalSelector);
          var componentTag = $modal[0].tagName.toLowerCase();
          var controller = $modal.controller(componentTag);

          if (angular.isFunction(controller.showModal)) {
            controller.showModal();
          }
        });
      }
    }
  });

  function SignupModalCtrl($scope, $http, $element) {
    var STEP_SIGNING_UP = 1;
    var STEP_COMPLETE = 2;
    var $modal = $element.find('.modal');

    this.step = 1;
    this.isBusy = false;
    this.newUser = {};
    this.serverMessage = null;

    this.showModal = function () {
      this.step = 1;

      this.isBusy = false;
      this.newUser = {};
      this.serverMessage = null;

      // $scope.$$childTail.formNewUser.$setPristine();
      $modal.modal('show');
    };

    this.onSignupClick = function () {
      var self = this;
      this.isBusy = true;

      var newUser = this.newUser;
      if (validate(newUser)) {
        $http.post('/api/signup', {
          username: newUser.username,
          password: newUser.password
        }).then(
          function (res) {
            self.step = STEP_COMPLETE;
          },
          function (err) {
            self.serverMessage = err.data;
          }
        ).finally(function () {
          self.isBusy = false;
        });
      }
    };

    this.onCloseClick = function () {
      $modal.modal('hide');
    };

    this.onLogInClick = function () {
      $modal.modal('hide');

      $('#login-modal').modal('show');
    };

    function validate (newUser) {
      return true;
    }
  }

  erbidCore.component('erbidSignupModal', {
    templateUrl: '/assets/partials/erbid-signup-modal.html',
    controller: SignupModalCtrl
  });


  function LoginCtrl($scope, $http, $sessionStorage, $window) {
    $scope.username = '';
    $scope.password = '';

    $scope.onLoginClick = function () {


      $http.post('/api/login',
        {
          username: $scope.username,
          password: $scope.password
        }
      ).then(function (res) {
        var data = res.data;

        if (data.ok === true) {
          $sessionStorage.authtoken = data.authtoken;
          $window.location.href = '/app';
        }
        else {
          alert('Cannot login');
        }
      }).catch(function (err) {
        console.error(err);
      })
    };
  }

  LoginCtrl.$inject = ['$scope', '$http', '$sessionStorage', '$window'];
  erbidCore.controller('LoginCtrl', LoginCtrl);


  erbidCore.component('erbidBidList', {
    templateUrl: '/assets/partials/erbid-bid-list.html',
    controller: BidListController
  });
  function BidListController($scope, erbidListings) {
    $scope.bids = [];

    this.$postLink = function () {
      this.getActiveBid().then(function (bids) {
        $scope.bids = bids;
      });
    };

    this.getActiveBid = function () {
      return erbidListings.get().$promise.then(function (resource) {
        return resource.listings;
      });
    };
  }

  BidListController.$inject = ['$scope', 'erbid.listings'];


  erbidCore.service('erbid.listings', ['$resource', function ($resource) {
    return $resource('/api/resources/listings');
  }]);
})(angular.module('erbid.core', ['ngResource', 'ngStorage']));
