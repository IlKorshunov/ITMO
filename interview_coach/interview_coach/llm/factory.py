from ..config import config, LLMProvider
from .base_llm import BaseLLMProvider


def create_llm_provider(provider: LLMProvider = None, **kwargs) -> BaseLLMProvider:
    """
    Создаёт LLM провайдер по конфигурации.
    
    Аргументы:
        provider: Тип провайдера (по умолчанию из конфига)
        **kwargs: Дополнительные параметры провайдера
    """
    provider = provider or config.llm.provider
    
    if provider == LLMProvider.HUGGINGFACE_API:
        from .langchain_provider import LangChainHuggingFaceProvider
        return LangChainHuggingFaceProvider(
            api_token=kwargs.get("api_token", config.llm.hf_api_token),
            model=kwargs.get("model", config.llm.hf_model),
        )
    
    elif provider == LLMProvider.LOCAL:
        from .huggingface_provider import LocalLLMProvider
        return LocalLLMProvider(
            base_url=kwargs.get("base_url", config.llm.local_model_url),
            model=kwargs.get("model", "local-model"),
            api_key=kwargs.get("api_key"),
        )