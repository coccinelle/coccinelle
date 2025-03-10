m2n::PtrM2N M2NConfiguration::getM2N(const std::string &acceptor, const std::string &connector)
{
  using std::get;
  for (M2NTuple &tuple : _m2ns) {
    if ((get<1>(tuple) == acceptor) && (get<4>(tuple) == connector)) {
      return get<0>(tuple);
    } else if ((get<4>(tuple) == acceptor) && (get<1>(tuple) == connector)) {
      return get<0>(tuple);
    }
  }
  PRECICE_ERROR("There is no m2n communication configured between participants \"" + acceptor + "\" and \"" + connector + "\". Please add an appropriate \"<m2n />\" tag.");
}
