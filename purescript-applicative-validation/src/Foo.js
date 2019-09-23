exports._makeValidFoo = function () {
  return {
    username: "foo",
    // >= 6
    password: "foobarbaz",
    // > 17
    age: 33,
  };
};

exports._makeInvalidFoo = function () {
  return {
    username: "bar",
    password: "bar",
    age: undefined
  };
};
