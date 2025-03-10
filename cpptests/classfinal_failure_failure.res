class FreeEnergyPerturbationData::Element final :
     public ISimulatorElement,
     public ICheckpointHelperClient,
     public IDomDecHelperClient
 {
 public:
  int y;
 };
