"""
Thermostat example for CEREBRUM framework.

This module implements a simple thermostat model that regulates temperature
through active inference and demonstrates case transformations.
"""

import numpy as np
from typing import Dict, Any, List, Optional, Tuple, Union
import sys

# Handle imports properly whether file is run directly or imported as a module
try:
    # Try relative imports (work when imported as a module)
    from ..core.model import Case, Model
    from ..core.active_inference import ActiveInferenceModel
except ImportError:
    # Fall back to absolute imports (work when run directly)
    from src.core.model import Case, Model
    from src.core.active_inference import ActiveInferenceModel

# Handle transformations imports
try:
    from ..transformations.case_transformations import transform_case
except ImportError:
    from src.transformations.case_transformations import transform_case

class ThermostatModel(ActiveInferenceModel):
    """
    A generative model of a thermostat that perceives and regulates temperature.
    
    This model demonstrates the case-bearing properties of the CEREBRUM framework
    through a simple homeostatic control scenario.
    """
    
    def __init__(
        self,
        name: str = "Thermostat",
        target_temperature: float = 22.0,
        temperature_precision: float = 10.0,
        action_precision: float = 1.0,
        temperature_noise: float = 0.1,
        min_temperature: float = 10.0,
        max_temperature: float = 30.0
    ):
        """
        Initialize the thermostat model.
        
        Args:
            name: Model name
            target_temperature: Desired temperature setpoint (in Celsius)
            temperature_precision: Precision of temperature sensing
            action_precision: Precision of temperature control actions
            temperature_noise: Noise level in temperature sensing
            min_temperature: Minimum allowable temperature
            max_temperature: Maximum allowable temperature
        """
        # Set up the model with 2 states: temperature and control action
        prior_means = np.array([target_temperature, 0.0])
        prior_precision = np.diag([1.0, action_precision])
        likelihood_precision = np.diag([temperature_precision])
        
        super().__init__(
            name=name,
            parameters={
                "target_temperature": target_temperature,
                "temperature_precision": temperature_precision,
                "action_precision": action_precision,
                "temperature_noise": temperature_noise,
                "min_temperature": min_temperature,
                "max_temperature": max_temperature
            },
            prior_means=prior_means,
            prior_precision=prior_precision,
            likelihood_precision=likelihood_precision
        )
        
        # Thermostat operational state
        self.current_temperature = target_temperature
        self.current_action = 0.0  # 0 = no action, negative = cooling, positive = heating
        
        # History of temperature and actions
        self.temperature_history = [self.current_temperature]
        self.action_history = [self.current_action]
        
        # Environment model for simulation
        self.ambient_temperature = 15.0
        self.thermal_inertia = 0.1  # How quickly temperature changes
        
        # Energy usage tracking
        self.energy_used = 0.0
        
        # Enhanced case configuration for thermostat
        self._setup_thermostat_case_configurations()
    
    def _setup_thermostat_case_configurations(self):
        """Set up thermostat-specific case configurations."""
        # NOMINATIVE case: actively controls temperature
        self._case_configurations[Case.NOMINATIVE].update({
            "functions_enabled": ["sense_temperature", "control_temperature"],
            "sensor_active": True,
            "actuator_active": True,
            "display_active": True,
            "parameter_focus": "control"
        })
        
        # ACCUSATIVE case: receives updates to its parameters
        self._case_configurations[Case.ACCUSATIVE].update({
            "functions_enabled": ["receive_update"],
            "sensor_active": False,
            "actuator_active": False,
            "display_active": True,
            "parameter_focus": "learning"
        })
        
        # GENITIVE case: generates temperature reports
        self._case_configurations[Case.GENITIVE].update({
            "functions_enabled": ["generate_report"],
            "sensor_active": True,
            "actuator_active": False,
            "display_active": True,
            "parameter_focus": "reporting"
        })
        
        # DATIVE case: receives temperature data
        self._case_configurations[Case.DATIVE].update({
            "functions_enabled": ["receive_temperature"],
            "sensor_active": False,
            "actuator_active": False,
            "display_active": True,
            "parameter_focus": "reception"
        })
        
        # INSTRUMENTAL case: used as a computational tool
        self._case_configurations[Case.INSTRUMENTAL].update({
            "functions_enabled": ["predict_temperature", "simulate_scenario"],
            "sensor_active": False,
            "actuator_active": False,
            "display_active": False,
            "parameter_focus": "simulation"
        })
        
        # LOCATIVE case: provides environmental context
        self._case_configurations[Case.LOCATIVE].update({
            "functions_enabled": ["provide_context"],
            "sensor_active": True,
            "actuator_active": False,
            "display_active": True,
            "parameter_focus": "environment"
        })
        
        # ABLATIVE case: serves as historical data source
        self._case_configurations[Case.ABLATIVE].update({
            "functions_enabled": ["provide_history"],
            "sensor_active": False,
            "actuator_active": False,
            "display_active": True,
            "parameter_focus": "history"
        })
        
        # VOCATIVE case: responds to direct commands
        self._case_configurations[Case.VOCATIVE].update({
            "functions_enabled": ["receive_command"],
            "sensor_active": False,
            "actuator_active": True,
            "display_active": True,
            "parameter_focus": "interface"
        })
    
    def likelihood(self, states: np.ndarray) -> np.ndarray:
        """
        Map from hidden states to expected observations.
        
        For the thermostat, this maps from the first state (temperature)
        to the expected temperature observation.
        
        Args:
            states: Hidden states of the model [temperature, action]
            
        Returns:
            Expected temperature observation
        """
        # Simple mapping: expected observation is the temperature state
        return np.array([states[0]])
    
    def transition_dynamics(self, states: np.ndarray, action: float) -> np.ndarray:
        """
        Model the dynamics of temperature change based on current state and action.
        
        Args:
            states: Current states [temperature, action]
            action: Control action (positive = heating, negative = cooling)
            
        Returns:
            Expected next states
        """
        temperature, current_action = states
        
        # Physical model of temperature dynamics
        # - Temperature moves toward ambient if no action
        # - Heating/cooling actions push temperature in corresponding direction
        # - Thermal inertia controls the rate of change
        
        ambient_effect = self.thermal_inertia * (self.ambient_temperature - temperature)
        action_effect = self.thermal_inertia * 2.0 * action
        
        next_temperature = temperature + ambient_effect + action_effect
        
        # Ensure temperature stays within allowed range
        next_temperature = max(self.parameters["min_temperature"], 
                             min(self.parameters["max_temperature"], next_temperature))
        
        # The action is also part of the state
        next_states = np.array([next_temperature, action])
        
        return next_states
    
    def update_environment(self, action: float) -> float:
        """
        Update the simulated environment based on the control action.
        
        Args:
            action: Control action (positive = heating, negative = cooling)
            
        Returns:
            New temperature after applying action
        """
        # Update energy usage
        self.energy_used += abs(action)
        
        # Apply the action and environmental dynamics
        new_states = self.transition_dynamics(
            np.array([self.current_temperature, self.current_action]),
            action
        )
        
        # Update current state
        self.current_temperature = new_states[0]
        self.current_action = action
        
        # Update history
        self.temperature_history.append(self.current_temperature)
        self.action_history.append(self.current_action)
        
        return self.current_temperature
    
    def sense_temperature(self, add_noise: bool = True) -> float:
        """
        Sense the current temperature, potentially with noise.
        
        Args:
            add_noise: Whether to add sensing noise
            
        Returns:
            Sensed temperature
        """
        if add_noise:
            noise = np.random.normal(0, self.parameters["temperature_noise"])
            return self.current_temperature + noise
        return self.current_temperature
    
    def control_temperature(self) -> float:
        """
        Compute and apply the optimal control action using active inference.
        
        Returns:
            The control action that was applied
        """
        # Sense current temperature (with noise)
        sensed_temp = self.sense_temperature()
        
        # Use active inference to determine optimal action
        temp_error = self.parameters["target_temperature"] - sensed_temp
        
        # Simple proportional control based on prediction error
        # This is a simplified approximation of active inference control
        action = 0.1 * temp_error
        
        # Apply the action to the environment
        self.update_environment(action)
        
        return action
    
    def simulate_step(self, action: Optional[float] = None) -> Dict[str, Any]:
        """
        Simulate one timestep of the thermostat's operation.
        
        Args:
            action: Optional manual action to apply (overrides normal control)
            
        Returns:
            Dictionary with simulation results
        """
        if action is None:
            # Use active inference control if no action specified
            action = self.control_temperature()
        else:
            # Apply the specified action
            self.update_environment(action)
        
        return {
            "temperature": self.current_temperature,
            "action": action,
            "target": self.parameters["target_temperature"],
            "energy": self.energy_used
        }
    
    def simulate_scenario(self, 
                        steps: int, 
                        ambient_temperatures: Optional[List[float]] = None,
                        target_changes: Optional[Dict[int, float]] = None) -> Dict[str, Any]:
        """
        Simulate a scenario over multiple timesteps.
        
        Args:
            steps: Number of steps to simulate
            ambient_temperatures: Optional list of ambient temps for each step
            target_changes: Optional dict mapping steps to new target temperatures
            
        Returns:
            Dictionary with simulation results
        """
        results = {
            "temperature": [],
            "action": [],
            "target": [],
            "energy": [],
            "ambient": []
        }
        
        for step in range(steps):
            # Update ambient temperature if specified
            if ambient_temperatures and step < len(ambient_temperatures):
                self.ambient_temperature = ambient_temperatures[step]
            
            # Update target temperature if specified
            if target_changes and step in target_changes:
                self.parameters["target_temperature"] = target_changes[step]
                # Also update prior mean for the target
                self.prior_means[0] = target_changes[step]
            
            # Run one step of simulation
            step_result = self.simulate_step()
            
            # Record results
            results["temperature"].append(step_result["temperature"])
            results["action"].append(step_result["action"])
            results["target"].append(step_result["target"])
            results["energy"].append(step_result["energy"])
            results["ambient"].append(self.ambient_temperature)
        
        return results
    
    # Implementation of case-specific update methods
    
    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """
        Update for NOMINATIVE case: thermostat as active temperature controller.
        
        Args:
            data: Optional manual control parameters
            
        Returns:
            Control action results
        """
        # In NOMINATIVE case, the thermostat actively controls temperature
        steps = 1
        if isinstance(data, dict) and "steps" in data:
            steps = data["steps"]
        
        results = {}
        for _ in range(steps):
            action = self.control_temperature()
            results = {
                "status": "success",
                "temperature": self.current_temperature,
                "action": action,
                "target": self.parameters["target_temperature"],
                "error": self.parameters["target_temperature"] - self.current_temperature,
                "case": "NOMINATIVE"
            }
        
        return results
    
    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """
        Update for ACCUSATIVE case: thermostat receiving parameter updates.
        
        Args:
            data: Dictionary of parameters to update
            
        Returns:
            Update results
        """
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary of parameters", "case": "ACCUSATIVE"}
        
        # Track which parameters were updated
        updated_params = {}
        
        # Update parameters
        for param, value in data.items():
            if param in self.parameters:
                old_value = self.parameters[param]
                self.parameters[param] = value
                updated_params[param] = {"old": old_value, "new": value}
                
                # Special handling for target temperature
                if param == "target_temperature":
                    self.prior_means[0] = value
        
        return {
            "status": "success",
            "updated_parameters": updated_params,
            "case": "ACCUSATIVE"
        }
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """
        Update for GENITIVE case: thermostat generating temperature reports.
        
        Args:
            data: Optional report configuration
            
        Returns:
            Generated report
        """
        # Determine report type
        report_type = "standard"
        if isinstance(data, dict) and "report_type" in data:
            report_type = data["report_type"]
        
        if report_type == "standard":
            # Generate standard temperature report
            report = {
                "current_temperature": self.current_temperature,
                "target_temperature": self.parameters["target_temperature"],
                "deviation": self.current_temperature - self.parameters["target_temperature"],
                "action": self.current_action,
                "energy_used": self.energy_used
            }
        elif report_type == "historical":
            # Generate historical temperature report
            report = {
                "temperature_history": self.temperature_history,
                "action_history": self.action_history,
                "current_temperature": self.current_temperature,
                "temperature_stats": {
                    "mean": np.mean(self.temperature_history),
                    "min": np.min(self.temperature_history),
                    "max": np.max(self.temperature_history),
                    "std": np.std(self.temperature_history)
                }
            }
        elif report_type == "efficiency":
            # Generate efficiency report
            avg_deviation = np.mean([abs(t - self.parameters["target_temperature"]) 
                                   for t in self.temperature_history])
            report = {
                "energy_used": self.energy_used,
                "avg_temperature_deviation": avg_deviation,
                "efficiency_score": 1.0 / (1.0 + self.energy_used * avg_deviation),
                "time_at_target": sum(1 for t in self.temperature_history 
                                     if abs(t - self.parameters["target_temperature"]) < 0.5)
            }
        else:
            report = {"error": f"Unknown report type: {report_type}"}
        
        return {
            "status": "success",
            "report": report,
            "report_type": report_type,
            "case": "GENITIVE"
        }
    
    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """
        Update for DATIVE case: thermostat receiving temperature data.
        
        Args:
            data: Temperature data to receive
            
        Returns:
            Data reception results
        """
        if data is None:
            return {"status": "error", "message": "No data provided", "case": "DATIVE"}
        
        received_data = {}
        
        if isinstance(data, (int, float)):
            # Single temperature value
            received_data["temperature"] = data
            # Update current temperature without using a control action
            self.current_temperature = data
            self.temperature_history.append(data)
            
        elif isinstance(data, dict):
            # Dictionary of values
            received_data = data.copy()
            
            if "temperature" in data:
                self.current_temperature = data["temperature"]
                self.temperature_history.append(data["temperature"])
            
            if "ambient" in data:
                self.ambient_temperature = data["ambient"]
        
        return {
            "status": "success",
            "received_data": received_data,
            "case": "DATIVE"
        }
    
    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """
        Update for INSTRUMENTAL case: thermostat used as a computational tool.
        
        Args:
            data: Dictionary with simulation parameters
            
        Returns:
            Simulation results
        """
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary of parameters", "case": "INSTRUMENTAL"}
        
        # Save current state to restore after simulation
        saved_state = {
            "temperature": self.current_temperature,
            "action": self.current_action,
            "ambient": self.ambient_temperature,
            "target": self.parameters["target_temperature"],
            "temp_history": self.temperature_history.copy(),
            "action_history": self.action_history.copy(),
            "energy": self.energy_used
        }
        
        try:
            # Extract simulation parameters
            method = data.get("method", "simulate")
            
            if method == "simulate":
                # Run a simulation scenario
                steps = data.get("steps", 10)
                ambient_temps = data.get("ambient_temperatures", None)
                target_changes = data.get("target_changes", None)
                
                # Run the simulation
                results = self.simulate_scenario(steps, ambient_temps, target_changes)
                
                return {
                    "status": "success",
                    "method": "simulate",
                    "results": results,
                    "case": "INSTRUMENTAL"
                }
            
            elif method == "predict_temperature":
                # Predict future temperature without running a full simulation
                current_temp = data.get("temperature", self.current_temperature)
                action = data.get("action", 0.0)
                ambient = data.get("ambient", self.ambient_temperature)
                steps = data.get("steps", 1)
                
                # Temporarily set the environment
                self.current_temperature = current_temp
                self.ambient_temperature = ambient
                
                # Predict future temperatures
                predictions = []
                for _ in range(steps):
                    next_states = self.transition_dynamics(
                        np.array([self.current_temperature, action]),
                        action
                    )
                    self.current_temperature = next_states[0]
                    predictions.append(self.current_temperature)
                
                return {
                    "status": "success",
                    "method": "predict_temperature",
                    "predictions": predictions,
                    "case": "INSTRUMENTAL"
                }
            
            else:
                return {"status": "error", "message": f"Unknown method: {method}", "case": "INSTRUMENTAL"}
                
        finally:
            # Restore original state
            self.current_temperature = saved_state["temperature"]
            self.current_action = saved_state["action"]
            self.ambient_temperature = saved_state["ambient"]
            self.parameters["target_temperature"] = saved_state["target"]
            self.temperature_history = saved_state["temp_history"]
            self.action_history = saved_state["action_history"]
            self.energy_used = saved_state["energy"]
    
    def _update_locative(self, data: Any) -> Dict[str, Any]:
        """
        Update for LOCATIVE case: thermostat providing contextual environment.
        
        Args:
            data: Optional context parameters to include
            
        Returns:
            Environmental context
        """
        # Define the environmental context
        context = {
            "ambient_temperature": self.ambient_temperature,
            "current_temperature": self.current_temperature,
            "thermal_inertia": self.thermal_inertia,
            "target_temperature": self.parameters["target_temperature"],
            "min_temperature": self.parameters["min_temperature"],
            "max_temperature": self.parameters["max_temperature"]
        }
        
        # Update context if requested
        if isinstance(data, dict):
            for key, value in data.items():
                if key == "ambient_temperature":
                    self.ambient_temperature = value
                    context["ambient_temperature"] = value
                elif key == "thermal_inertia":
                    self.thermal_inertia = value
                    context["thermal_inertia"] = value
        
        return {
            "status": "success",
            "context": context,
            "case": "LOCATIVE"
        }
    
    def _update_ablative(self, data: Any) -> Dict[str, Any]:
        """
        Update for ABLATIVE case: thermostat as source of historical data.
        
        Args:
            data: Optional parameters for history retrieval
            
        Returns:
            Historical data
        """
        # Default to returning all history
        start_idx = 0
        end_idx = len(self.temperature_history)
        
        # Process data parameters if provided
        if isinstance(data, dict):
            if "start" in data:
                start_idx = max(0, min(len(self.temperature_history) - 1, data["start"]))
            if "end" in data:
                end_idx = max(start_idx + 1, min(len(self.temperature_history), data["end"]))
        
        # Extract the requested history
        history = {
            "temperature": self.temperature_history[start_idx:end_idx],
            "action": self.action_history[start_idx:min(end_idx, len(self.action_history))],
            "time_range": [start_idx, end_idx]
        }
        
        # Add statistical analysis if requested
        if isinstance(data, dict) and data.get("include_stats", False):
            temp_data = np.array(history["temperature"])
            action_data = np.array(history["action"])
            
            history["statistics"] = {
                "temperature": {
                    "mean": np.mean(temp_data),
                    "std": np.std(temp_data),
                    "min": np.min(temp_data),
                    "max": np.max(temp_data)
                },
                "action": {
                    "mean": np.mean(action_data),
                    "std": np.std(action_data),
                    "min": np.min(action_data),
                    "max": np.max(action_data),
                    "total_energy": np.sum(np.abs(action_data))
                }
            }
        
        return {
            "status": "success",
            "history": history,
            "case": "ABLATIVE"
        }
    
    def _update_vocative(self, data: Any) -> Dict[str, Any]:
        """
        Update for VOCATIVE case: thermostat responding to direct commands.
        
        Args:
            data: Command string or dictionary
            
        Returns:
            Command response
        """
        if data is None:
            return {
                "status": "success",
                "response": f"Thermostat {self.name} ready for commands",
                "case": "VOCATIVE"
            }
        
        if isinstance(data, str):
            # Process string commands
            cmd = data.lower()
            
            if cmd == "status":
                return {
                    "status": "success",
                    "response": {
                        "temperature": self.current_temperature,
                        "target": self.parameters["target_temperature"],
                        "action": self.current_action,
                        "energy": self.energy_used
                    },
                    "case": "VOCATIVE"
                }
            
            elif cmd == "increase_temp" or cmd == "warmer":
                new_target = self.parameters["target_temperature"] + 1.0
                self.parameters["target_temperature"] = new_target
                self.prior_means[0] = new_target
                
                return {
                    "status": "success",
                    "response": f"Target temperature increased to {new_target}°C",
                    "case": "VOCATIVE"
                }
            
            elif cmd == "decrease_temp" or cmd == "cooler":
                new_target = self.parameters["target_temperature"] - 1.0
                self.parameters["target_temperature"] = new_target
                self.prior_means[0] = new_target
                
                return {
                    "status": "success",
                    "response": f"Target temperature decreased to {new_target}°C",
                    "case": "VOCATIVE"
                }
            
            elif cmd == "help":
                return {
                    "status": "success",
                    "response": "Available commands: status, increase_temp/warmer, decrease_temp/cooler, set_temp, "
                              "energy_report, reset_energy",
                    "case": "VOCATIVE"
                }
            
            else:
                return {
                    "status": "error",
                    "response": f"Unknown command: {cmd}",
                    "case": "VOCATIVE"
                }
        
        elif isinstance(data, dict):
            # Process dictionary commands
            if "set_temp" in data:
                new_temp = float(data["set_temp"])
                new_temp = max(self.parameters["min_temperature"], 
                             min(self.parameters["max_temperature"], new_temp))
                
                self.parameters["target_temperature"] = new_temp
                self.prior_means[0] = new_temp
                
                return {
                    "status": "success",
                    "response": f"Target temperature set to {new_temp}°C",
                    "case": "VOCATIVE"
                }
            
            elif "reset_energy" in data:
                old_energy = self.energy_used
                self.energy_used = 0.0
                
                return {
                    "status": "success",
                    "response": f"Energy counter reset from {old_energy:.2f} to 0.0",
                    "case": "VOCATIVE"
                }
            
            elif "energy_report" in data:
                return {
                    "status": "success",
                    "response": {
                        "total_energy": self.energy_used,
                        "energy_per_degree": self.energy_used / max(1, len(self.temperature_history)),
                        "efficiency": 1.0 / (1.0 + self.energy_used * 
                                         np.mean([abs(t - self.parameters["target_temperature"]) 
                                                for t in self.temperature_history]))
                    },
                    "case": "VOCATIVE"
                }
        
        return {
            "status": "error",
            "response": "Unrecognized command format",
            "case": "VOCATIVE"
        }


def demonstrate_thermostat_cases():
    """
    Demonstrate the thermostat model in all cases.
    
    This function creates a thermostat model and demonstrates
    its behavior in each of the linguistic cases.
    
    Returns:
        Dictionary with results from each case demonstration
    """
    # Create a thermostat model
    thermostat = ThermostatModel(
        name="HomeThermo",
        target_temperature=22.0,
        temperature_precision=10.0,
        temperature_noise=0.1
    )
    
    # Dictionary to store results from each case
    results = {}
    
    # Demonstrate NOMINATIVE case: thermostat as active controller
    transform_case(thermostat, Case.NOMINATIVE)
    results["NOMINATIVE"] = thermostat.update({"steps": 5})
    
    # Demonstrate ACCUSATIVE case: thermostat receiving parameter updates
    transform_case(thermostat, Case.ACCUSATIVE)
    results["ACCUSATIVE"] = thermostat.update({"target_temperature": 23.5})
    
    # Demonstrate GENITIVE case: thermostat generating reports
    transform_case(thermostat, Case.GENITIVE)
    results["GENITIVE"] = thermostat.update({"report_type": "standard"})
    
    # Demonstrate DATIVE case: thermostat receiving temperature data
    transform_case(thermostat, Case.DATIVE)
    results["DATIVE"] = thermostat.update({"temperature": 21.0, "ambient": 18.0})
    
    # Demonstrate INSTRUMENTAL case: thermostat as computational tool
    transform_case(thermostat, Case.INSTRUMENTAL)
    results["INSTRUMENTAL"] = thermostat.update({
        "method": "simulate",
        "steps": 10,
        "ambient_temperatures": [18.0] * 10,
        "target_changes": {5: 24.0}
    })
    
    # Demonstrate LOCATIVE case: thermostat providing environmental context
    transform_case(thermostat, Case.LOCATIVE)
    results["LOCATIVE"] = thermostat.update({"ambient_temperature": 16.0})
    
    # Demonstrate ABLATIVE case: thermostat as historical data source
    transform_case(thermostat, Case.ABLATIVE)
    results["ABLATIVE"] = thermostat.update({"include_stats": True})
    
    # Demonstrate VOCATIVE case: thermostat responding to commands
    transform_case(thermostat, Case.VOCATIVE)
    results["VOCATIVE"] = thermostat.update("status")
    
    return results


if __name__ == "__main__":
    # Demonstrate the thermostat model in different cases
    results = demonstrate_thermostat_cases()
    
    # Print the results
    for case, result in results.items():
        print(f"\n=== {case} Case Results ===")
        print(result) 